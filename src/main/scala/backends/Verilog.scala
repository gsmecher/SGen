/*
 *    _____ ______          SGen - A Generator of Streaming Hardware
 *   / ___// ____/__  ____  Department of Computer Science, ETH Zurich, Switzerland
 *   \__ \/ / __/ _ \/ __ \
 *  ___/ / /_/ /  __/ / / / Copyright (C) 2020-2025 François Serre (serref@inf.ethz.ch)
 * /____/\____/\___/_/ /_/  https://github.com/fserre/sgen
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA
 *
 */

package backends
import ir.rtl.*
import ir.rtl.hardwaretype.*
import transforms.Transform

import scala.collection.immutable.HashMap

// import scala.collection.{immutable, mutable}
import scala.annotation.tailrec

/**
 * Adds a Verilog backend to modules.
 */
object Verilog {
  extension (mod:Module)
    /**
     * @return a string containing the verilog code of the module
     */
    def toVerilog: String =
      // Get IDs for each regular RTL nodes. An RTL node may use several ids.
      val indexes = HashMap.from(mod.components.zip(mod.components.map {
        case _: Input | _: Output | _: Wire | _: Const => 0
        case Register(_, cycles) if cycles > 1 => 2
        case _: RAM | _: BlockROM => 2
        case _ => 1
      }.scanLeft(1)(_ + _)))

      // Returns a verilog identifier for each of the nodes
      @tailrec
      def getName(comp: Component, internal: Int = 0): String = comp match
        case Input(_,name) => name
        case Output(_,name) => name
        case Wire(input) => getName(input)
        case Const(size, value) => s"$size'd$value"
        case _ => s"s${indexes(comp) + internal}"


      val declarations = (mod.components.flatMap {
        case _: Output | _: Input | _: Const | _: Wire => Seq()
        case cur@Mux(address, inputs) if address.size > 1 => Seq(s"reg ${if (cur.size != 1) s"[${cur.size - 1}:0] " else ""}${getName(cur)};")
        case cur@Register(_, cycles) if cycles == 1 => Seq(s"reg ${if (cur.size != 1) s"[${cur.size - 1}:0] " else ""}${getName(cur)};")
        case cur@Register(_, cycles) if cycles == 2 => Seq(
          s"reg ${if (cur.size != 1) s"[${cur.size - 1}:0] " else ""}${getName(cur, 1)};",
          s"reg ${if (cur.size != 1) s"[${cur.size - 1}:0] " else ""}${getName(cur)};")
        case cur@Register(_, cycles) => Seq(
          s"reg ${if (cur.size != 1) s"[${cur.size - 1}:0] " else ""}${getName(cur, 1)} [${cycles - 1}:0];",
          s"wire ${if (cur.size != 1) s"[${cur.size - 1}:0] " else ""}${getName(cur)};")
        case cur: RAM => Seq(s"wire ${if (cur.size != 1) s"[${cur.size - 1}:0] " else ""}${getName(cur)};")
        case cur: BlockROM => Seq(s"wire ${if (cur.size != 1) s"[${cur.size - 1}:0] " else ""}${getName(cur)};")
        case cur => Seq(s"wire ${if (cur.size != 1) s"[${cur.size - 1}:0] " else ""}${getName(cur)};")
      }:+"integer i;":+"reg ram_we = 1'b0; // write enable of the RAMs (see below)").map(s => s"  $s\n").mkString("")
      val resetName = mod.inputs.collectFirst { case Input(_, name) if name == "reset" => name }.getOrElse("1'b0")

      val assignments = mod.components.flatMap(cur => (cur match
        case Output(input, _) => Some(getName(input))
        case Plus(terms) => Some(terms.map(getName(_)).mkString(" + "))
        case Minus(lhs, rhs) => Some(s"${getName(lhs)} - ${getName(rhs)}")
        case Times(lhs, rhs) => Some(s"$$signed(${getName(lhs)}) * $$signed(${getName(rhs)})")
        case And(terms) => Some(terms.map(getName(_)).mkString(" & "))
        case Xor(inputs) => Some(inputs.map(getName(_)).mkString(" ^ "))
        case Or(inputs) => Some(inputs.map(getName(_)).mkString(" | "))
        case Equals(lhs, rhs) => Some(s"${getName(lhs)} == ${getName(rhs)}")
        case Not(input) => Some(s"~${getName(input)}")
        case Concat(inputs) => Some(inputs.map(getName(_)).mkString("{",", ","}"))
        case Tap(input, range) => Some(s"${getName(input)}[${if (range.size > 1) s"${range.last}:" else ""}${range.start}]")
        case Register(input, cycles) if cycles > 2 => Some(s"${getName(cur,1)} [${cycles - 1}]")
        case Mux(address, inputs) if address.size == 1 => Some(s"${getName(address)} ? ${getName(inputs.last)} : ${getName(inputs.head)}")
        case _ => None
      ).map((cur, _))).map((cur, rhs) => s"  assign ${getName(cur)} = $rhs;\n").mkString("")

      val sequential = (s"ram_we <= ~$resetName;" +: mod.components.flatMap {
        case cur@Register(input, cycles) if cycles == 1 => Seq(s"${getName(cur)} <= ${getName(input)};")
        case cur@Register(input, cycles) if cycles == 2 => Seq(
          s"${getName(cur, 1)} <= ${getName(input)};",
          s"${getName(cur)} <= ${getName(cur, 1)};")
        case cur@Register(input, cycles) => Seq(
          s"${getName(cur, 1)} [0] <= ${getName(input)};",
          s"for (i = 1; i < $cycles; i = i + 1)",
          s"  ${getName(cur, 1)} [i] <= ${getName(cur, 1)} [i - 1];")
        case _ => Seq()
      }).map(s => s"      $s\n").mkString("")

      val combinatorial = mod.components.flatMap {
        case cur@Mux(address, inputs) if address.size > 1 =>
          "always @(*)" +:
            s"  case(${getName(address)})" +:
            inputs.zipWithIndex.map((in, i) => s"    ${if (i == inputs.size - 1 && ((1 << address.size) != inputs.size)) "default" else i}: ${getName(cur)} = ${getName(in)};") :+
            "  endcase"
        case cur@RAM(data, wr, rd) =>
          // Block RAMs use NO_CHANGE mode, which supports a higher clock frequency than READ_FIRST on UltraScale devices, but requires that
          // the two ports never access the same address in the same cycle (true for the permutations generated by SGen, which read an
          // address before rewriting it). Vivado maps an XPM RAM whose write enable is a constant to READ_FIRST whatever the write mode
          // says, hence the registered enable. Distributed RAMs only support READ_FIRST.
          val (primitive, writeMode) = if (1 << wr.size) >= RAM.blockDepth then ("block", "no_change") else ("distributed", "read_first")
          Seq(
          "xpm_memory_sdpram #(",
          s"  .ADDR_WIDTH_A(${wr.size}), .ADDR_WIDTH_B(${rd.size}), .WRITE_DATA_WIDTH_A(${cur.size}), .BYTE_WRITE_WIDTH_A(${cur.size}), .READ_DATA_WIDTH_B(${cur.size}),",
          s"  .MEMORY_SIZE(${cur.size << wr.size}), .MEMORY_PRIMITIVE(\"$primitive\"), .CLOCKING_MODE(\"common_clock\"),",
          s"  .READ_LATENCY_B(${RAM.readLatency}), .WRITE_MODE_B(\"$writeMode\"), .SIM_ASSERT_CHK(1)",
          s") ${getName(cur, 1)} (",
          s"  .clka(clk), .ena(1'b1), .wea(ram_we), .addra(${getName(wr)}), .dina(${getName(data)}),",
          s"  .clkb(clk), .enb(1'b1), .regceb(1'b1), .rstb(1'b0), .addrb(${getName(rd)}), .doutb(${getName(cur)}),",
          "  .sleep(1'b0), .injectsbiterra(1'b0), .injectdbiterra(1'b0), .sbiterrb(), .dbiterrb());")
        case cur@BlockROM(values, address, _) =>
          val cfg = BlockRAMConfig(values.size, cur.size).get
          blockMemory(getName(cur), getName(cur, 1), cfg, cur.size, address.size, getName(address), None, Some(values))
        case cur: Extern =>
          mod.dependencies.add(cur.filename)
          Seq(s"${cur.module} ext_${getName(cur)}(${cur.inputs.map { case (name, comp) => s".$name(${getName(comp)}), " }.mkString}.${cur.outputName}(${getName(cur)}));")
        case _ => Seq()
      }.map(s => s"  $s\n").mkString("")

      val result = new StringBuilder
      result ++= s"module main(input clk,\n"
      result ++= mod.inputs.map(s => s"  input ${if (s.size != 1) s"[${s.size - 1}:0] " else ""}${getName(s)},\n").mkString("")
      result ++= mod.outputs.map(s => s"  output ${if (s.size != 1) s"[${s.size - 1}:0] " else ""}${getName(s)}").mkString(",\n")
      result ++= ");\n\n"
      result ++= declarations
      result ++= assignments
      result ++= combinatorial
      if sequential.nonEmpty then
        result ++= "  always @(posedge clk)\n"
        result ++= "    begin\n"
        result ++= sequential
        result ++= "    end\n"
      result ++= "endmodule\n"
      result.toString()


  /**
   * Verilog for a memory made of directly instantiated block RAM primitives (see BlockRAMConfig), with a read latency of RAM.readLatency.
   * The primitives use their output register and the NO_CHANGE write mode (see RAM above). Used for ROMs: the XPM ROM macro only
   * supports READ_FIRST (which limits the clock frequency on UltraScale devices), and initialisation through a parameter is limited to 4 kbit.
   *
   * @param out Name of the output wire (already declared)
   * @param base Prefix for the names of the instances and internal wires
   * @param width Width of the words
   * @param addrWidth Width of the addresses
   * @param rd Read address
   * @param write Write address and data, or None for a ROM
   * @param init Content, for a ROM
   */
  private def blockMemory(out: String, base: String, cfg: BlockRAMConfig, width: Int, addrWidth: Int, rd: String, write: Option[(String, String)], init: Option[Seq[BigInt]]): Seq[String] =
    val slices = (width + cfg.width - 1) / cfg.width
    val (d, p, pinD, pinP) = (cfg.dataBits, cfg.parityBits, if cfg.prim == "RAMB18E2" then 16 else 32, if cfg.prim == "RAMB18E2" then 2 else 4)
    def addr(a: String) = s"{${if cfg.wordAddrBits > addrWidth then s"${cfg.wordAddrBits - addrWidth}'d0, " else ""}$a, ${cfg.addrShift}'d0}"
    def pad(expr: String, from: Int, to: Int) = if to > from then s"{${to - from}'d0, $expr}" else expr
    val nInit = if cfg.prim == "RAMB18E2" then 64 else 128
    val mask256 = (BigInt(1) << 256) - 1
    val wrData = write.map(_ => s"${base}_din")
    val header = write.map((_, data) => s"wire [${width - 1}:0] ${base}_din = $data;").toSeq
    header ++ (0 until slices).flatMap { i =>
      val lo = i * cfg.width; val w = math.min(width, lo + cfg.width) - lo
      val inst = s"${base}_$i"
      val din = s"${inst}_din"
      val dinExpr = wrData.map(n => pad(s"$n[${lo + w - 1}:$lo]", w, cfg.width)).getOrElse(s"${cfg.width}'d0")
      // Word layout: data bits in the low d bits, parity bits above. In simple dual-port mode a word spans both halves of the primitive.
      val (dinA, dinB, dinPA, dinPB) =
        if cfg.sdp then (s"$din[${d / 2 - 1}:0]", s"$din[${d - 1}:${d / 2}]", s"$din[${d + p / 2 - 1}:$d]", s"$din[${cfg.width - 1}:${d + p / 2}]")
        else (pad(s"$din[${d - 1}:0]", d, pinD), s"${pinD}'d0", pad(s"$din[${cfg.width - 1}:$d]", p, pinP), s"${pinP}'d0")
      val initAttrs = init.toSeq.flatMap { values =>
        val sliceValues = values.map(v => (v >> lo) & ((BigInt(1) << w) - 1))
        val (dataInit, parityInit) = sliceValues.zipWithIndex.foldLeft((BigInt(0), BigInt(0))) { case ((di, pi), (v, j)) => (di | ((v & ((BigInt(1) << d) - 1)) << (j * d)), pi | ((v >> d) << (j * p))) }
        (0 until nInit).map(j => (s"INIT_${"%02X".format(j)}", (dataInit >> (256 * j)) & mask256)) ++
        (0 until nInit / 8).map(j => (s"INITP_${"%02X".format(j)}", (parityInit >> (256 * j)) & mask256))
      }.filter(_._2 != 0).map((name, v) => s"  .$name(256'h${v.toString(16)}),")
      val (rdAddrPin, wrAddrPin, wea, webwe) =
        if cfg.sdp then ("ADDRARDADDR", "ADDRBWRADDR", if cfg.prim == "RAMB18E2" then "2'b00" else "4'b0000", if write.isDefined then (if cfg.prim == "RAMB18E2" then "4'b1111" else "8'b11111111") else (if cfg.prim == "RAMB18E2" then "4'b0000" else "8'b00000000"))
        else ("ADDRBWRADDR", "ADDRARDADDR", if write.isDefined then (if cfg.prim == "RAMB18E2" then "2'b11" else "4'b1111") else (if cfg.prim == "RAMB18E2" then "2'b00" else "4'b0000"), if cfg.prim == "RAMB18E2" then "4'b0000" else "8'b00000000")
      val widths = if cfg.sdp then s".READ_WIDTH_A(${cfg.width}), .READ_WIDTH_B(0), .WRITE_WIDTH_A(0), .WRITE_WIDTH_B(${cfg.width})" else s".READ_WIDTH_A(${cfg.width}), .READ_WIDTH_B(${cfg.width}), .WRITE_WIDTH_A(${cfg.width}), .WRITE_WIDTH_B(${cfg.width})"
      val q = if cfg.sdp then s"{${inst}_dopb[${p / 2 - 1}:0], ${inst}_dopa[${p / 2 - 1}:0], ${inst}_dob[${d / 2 - 1}:0], ${inst}_doa[${d / 2 - 1}:0]}" else s"{${inst}_dopb[${p - 1}:0], ${inst}_dob[${d - 1}:0]}"
      Seq(
        s"wire [${cfg.width - 1}:0] $din = $dinExpr;",
        s"wire [${pinD - 1}:0] ${inst}_doa, ${inst}_dob; wire [${pinP - 1}:0] ${inst}_dopa, ${inst}_dopb; wire [${cfg.width - 1}:0] ${inst}_q = $q;",
        s"${cfg.prim} #($widths, .WRITE_MODE_A(\"NO_CHANGE\"), .WRITE_MODE_B(\"NO_CHANGE\"), .DOA_REG(1), .DOB_REG(1),") ++ initAttrs ++ Seq(
        s"  .CLOCK_DOMAINS(\"COMMON\"), .CASCADE_ORDER_A(\"NONE\"), .CASCADE_ORDER_B(\"NONE\")) $inst (",
        s"  .CLKARDCLK(clk), .CLKBWRCLK(clk), .ENARDEN(1'b1), .ENBWREN(1'b1), .REGCEAREGCE(1'b1), .REGCEB(1'b1), .SLEEP(1'b0),",
        s"  .RSTRAMARSTRAM(1'b0), .RSTRAMB(1'b0), .RSTREGARSTREG(1'b0), .RSTREGB(1'b0), .ADDRENA(1'b1), .ADDRENB(1'b1),",
        s"  .$rdAddrPin(${addr(rd)}), .$wrAddrPin(${addr(write.map(_._1).getOrElse(s"${addrWidth}'d0"))}), .WEA($wea), .WEBWE($webwe),",
        s"  .DINADIN($dinA), .DINBDIN($dinB), .DINPADINP($dinPA), .DINPBDINP($dinPB),",
        s"  .DOUTADOUT(${inst}_doa), .DOUTBDOUT(${inst}_dob), .DOUTPADOUTP(${inst}_dopa), .DOUTPBDOUTP(${inst}_dopb),",
        s"  .CASDIMUXA(1'b0), .CASDIMUXB(1'b0), .CASDINA(${pinD}'b0), .CASDINB(${pinD}'b0), .CASDINPA(${pinP}'b0), .CASDINPB(${pinP}'b0), .CASDOMUXA(1'b0), .CASDOMUXB(1'b0),",
        s"  .CASDOMUXEN_A(1'b0), .CASDOMUXEN_B(1'b0), .CASOREGIMUXA(1'b0), .CASOREGIMUXB(1'b0), .CASOREGIMUXEN_A(1'b0), .CASOREGIMUXEN_B(1'b0)${if cfg.prim == "RAMB36E2" then ", .CASINDBITERR(1'b0), .CASINSBITERR(1'b0), .ECCPIPECE(1'b0), .INJECTDBITERR(1'b0), .INJECTSBITERR(1'b0)" else ""});")
    } :+ s"assign $out = {${(0 until slices).reverse.map(i => s"${base}_${i}_q[${math.min(width, (i + 1) * cfg.width) - 1 - i * cfg.width}:0]").mkString(", ")}};"

  extension [U](sm:StreamingModule[U]) {
    def getTestBench(transform: Transform[U]): String =
      val (testInputs, epsilon) = transform.testParams.applyOrElse(sm.hw,_ => throw Exception(s"Testbench is unavailable for ${sm.hw}."))
      getTestBench(testInputs, s"${transform} streaming with k=${sm.k} using ${sm.hw}", epsilon)

    /**
     * @param repeat Number of datasets that will be tested
     * @param addedGap Number of cycles to add between datasets, in addition to the gap required by the design
     * @returns A verilog testbench of the design
     */
    def getTestBench(inputs: Seq[U], description: String = "", epsilon: Double = 0, addedGap: Int = 0): String = {
      require(inputs.size % sm.N == 0)
      //val input = if(inputs.isEmpty) sm.testBenchInput(2) else inputs.map(sm.hw.bitsOf)
      val repeat = inputs.size / sm.N
      //val input = Vector.tabulate(repeat)(set => Vector.tabulate[Int](N)(i => i * 100 + set * 1000))
      //val input = Vector.tabulate(repeat)(set => Vector.tabulate[BigInt](N)(i => 0))


      val res = new StringBuilder
      res ++= "module test;\n"
      res ++= "    reg clk,rst,next;\n"
      sm.dataInputs.foreach(res ++= "    reg [" ++= (sm.hw.size - 1).toString ++= ":0] " ++= _.name ++= ";\n")
      res ++= "    wire next_out;\n"
      sm.dataOutputs.foreach(res ++= "    wire [" ++= (sm.hw.size - 1).toString ++= ":0] " ++= _.name ++= ";\n")
      res ++= "\n"
      res ++= " //Clock\n"
      res ++= "    always\n"
      res ++= "      begin\n"
      res ++= "        clk <= 0;#50;\n"
      res ++= "        clk <= 1;#50;\n"
      res ++= "      end\n"
      res ++= "\n"
      res ++= "//inputs\n"
      res ++= "    initial\n"
      res ++= "      begin\n"
      res ++= "        @(posedge clk);\n"
      res ++= "        next <= 0;\n"
      (0 to (sm.latency + sm.inputDelay - sm.nextAt + sm.T)).foreach(_ => res ++= "        @(posedge clk);\n")
      res ++= "        rst <= 1;\n"
      res ++= "        @(posedge clk);\n"
      res ++= "        @(posedge clk);\n"
      res ++= "        rst <= 0;\n"
      (Math.min(sm.nextAt, 0) until Math.max((sm.T + sm.minGap + addedGap) * repeat, (sm.T + sm.minGap + addedGap) * (repeat - 1) + sm.nextAt + 4)).foreach(cycle => {
        res ++= "        @(posedge clk); //cycle " ++= cycle.toString ++= "\n"
        if ((cycle - sm.nextAt) >= 0 && (cycle - sm.nextAt) % (sm.T + sm.minGap + addedGap) == 0 && (cycle - sm.nextAt) / (sm.T + sm.minGap + addedGap) < repeat)
          res ++= "        next <= 1;\n"
        if ((cycle - sm.nextAt + 1) >= 0 && (cycle - sm.nextAt) % (sm.T + sm.minGap + addedGap) == 1 && (cycle - sm.nextAt) / (sm.T + sm.minGap + addedGap) < repeat)
          res ++= "        next <= 0;\n"
        val set = cycle / (sm.T + sm.minGap + addedGap)
        val c = cycle % (sm.T + sm.minGap + addedGap)
        if (set < repeat && cycle >= 0 && c < sm.T) {
          if (c == 0)
            res ++= "        //dataset " + set + " enters.\n"
          sm.dataInputs.zipWithIndex.foreach(i => res ++= "        " ++= i._1.name ++= " <= " ++= sm.hw.size.toString ++= "'d" ++= sm.hw.bitsOf(inputs(set * sm.N + c * sm.K + i._2)).toString ++= "; //" ++= inputs(set * sm.N + c * sm.K + i._2).toString ++= "\n")
        }
      })

      res ++= "      end\n"
      res ++= s"    reg [${sm.hw.size/2-1}:0] tmp; // used to display results.\n"
      res ++= "    real tmpr; // used to display results.\n"
      res ++= "    real epsilon; // used to display results.\n"
      res ++= "    initial\n"
      res ++= "      begin\n"
      if description!="" then
        res ++= s"       $$display(\"Testing $description...\");\n"
      res ++= s"        $$display(\"Epsilon: $epsilon\");\n"
      res ++= "        @(posedge next_out);//#100;\n"
      res ++= "        #50;\n"
      val outputs = inputs.grouped(sm.N).toSeq.zipWithIndex.flatMap { case (input, set) => sm.spl.eval(input, set) }
      for r <- 0 until repeat do
        for c <- 0 until sm.T do
          for i <- 0 until sm.K do
            res ++= s"        $$write(\"output${r * sm.T * sm.K + c * sm.K + i}: %0d (\",${sm.dataOutputs(i).name});\n"
            res ++= s"        ${displayInVerilog(sm.hw, sm.dataOutputs(i).name)}\n"
            res ++= s"        $$display(\") expected: ${sm.hw.bitsOf(outputs(r * sm.N + c * sm.K + i)).toString} (${outputs(r * sm.N + c * sm.K + i)})\");\n"
            res ++= s"        ${callFinishIfDifferent(sm.hw, sm.dataOutputs(i).name, outputs(r * sm.N + c * sm.K + i), epsilon)}\n"
          res ++= s"        #100;\n"
        res ++= s"        #${100 * (sm.minGap + addedGap)}; //gap\n"
      res ++= "        $display(\"Success.\");\n"
      res ++= "        $finish();\n"
      res ++= "      end\n"
      res ++= "      main uut(clk,rst,next," ++= (0 until sm.K).map(i => sm.dataInputs(i).name).mkString(",") ++= ",next_out," ++= (0 until sm.K).map(i => sm.dataOutputs(i).name).mkString(",") ++= ");\n"
      res ++= "endmodule\n"
      res.toString
    }
  }

  private def displayInVerilog(hw: HW[?], varName: String): String = hw match
    case ComplexHW(hw) if !hw.isInstanceOf[ComplexHW[?]] => s"tmp=$varName[${hw.size - 1}:0]; ${displayInVerilog(hw, "tmp")} $$write(\" + \"); tmp=$varName[${2 * hw.size - 1}:${hw.size}]; ${displayInVerilog(hw, "tmp")} $$write(\"i\");"
    case FixedPoint(_, fractional) => s"$$write(\"%0f\",$$itor($$signed($varName))/${1 << fractional});"
    case Flopoco(wE, wF) if wE <= 11 && wF <= 52 => s"if($varName[${wE+wF}]) $$write(\"-\"); if($varName[${wE+wF+2}:${wE+wF+1}] == 0) $$write(\"0\"); else if($varName[${wE+wF+2}:${wE+wF+1}] == 2) $$write(\"infinity\"); else if($varName[${wE+wF+2}:${wE+wF+1}] == 3) $$write(\"NaN\"); else $$write(\"%0f\", $$bitstoreal({1'd0,11'd${(1<<10)-(1<<(wE-1))}+$varName[${wE+wF-1}:$wF],$varName[${wF-1}:0]${if wF<52 then s",${52-wF}'d0" else ""}}));"
    case IEEE754(wE, wF) if wE <= 11 && wF <= 52 => s"$$write(\"%0f\", $$bitstoreal({$varName[${wE+wF}],$varName[${wE+wF-1}:$wF] == 0 ? 11'd0 : $varName[${wE+wF-1}:$wF] == ${(1<<wE) - 1} ? 11'd2047 : 11'd${(1<<10)-(1<<(wE-1))} + $varName[${wE+wF-1}:$wF], $varName[${wF-1}:0]${if wF<52 then s", ${52-wF}'d0" else ""}}));"
    case _ => s"$$write(\"%0d\", $varName);"

  private def callFinishIfDifferent[T](hw:HW[T], varName: String, value: T, epsilon: Double): String = hw match
    case Flopoco(wE, wF) if wE <= 11 && wF <= 52 => s"tmpr = $varName[${wE+wF+2}:${wE+wF+1}] == 0?0:$$bitstoreal({$varName[${wE+wF}],11'd${(1<<10)-(1<<(wE-1))}+$varName[${wE+wF-1}:$wF],$varName[${wF-1}:0]${if wF<52 then s",${52-wF}'d0" else ""}}) - $$bitstoreal(64'h${java.lang.Long.toHexString(java.lang.Double.doubleToLongBits(value))}); epsilon = $$bitstoreal(64'h${java.lang.Long.toHexString(java.lang.Double.doubleToLongBits(epsilon))}); $$display(\"tmpr: %0f epsilon: %0f\",tmpr, epsilon); if(tmpr > epsilon || -tmpr > epsilon) $$finish;"
    case FixedPoint(magnitude, fractional) =>  s"if(($$signed($varName) > ${hw.size}'sd${hw.bitsOf(value)} ? $$signed($varName) - ${hw.size}'sd${hw.bitsOf(value)} : ${hw.size}'sd${hw.bitsOf(value)} - $$signed($varName)) > ${hw.size}'sd${hw.bitsOf(epsilon)}) $$finish;"
    case ComplexHW(hw) => s"tmp = $varName[${hw.size - 1}:0]; ${callFinishIfDifferent(hw,"tmp", value.re, epsilon)} tmp = $varName[${2 * hw.size - 1}:${hw.size}]; ${callFinishIfDifferent(hw,"tmp", value.im, epsilon)}"
    case Unsigned(size) => s"if(($varName > $size'd${hw.bitsOf(value)} ? $varName - $size'd${hw.bitsOf(value)} : $size'd${hw.bitsOf(value)} - $varName) > $size'd${hw.bitsOf(epsilon.toInt)}) $$finish;"
    case IEEE754(wE, wF) if wE <= 11 && wF <= 52 => s"tmpr = $$bitstoreal({$varName[${wE + wF}],$varName[${wE + wF - 1}:$wF] == 0 ? 11'd0 : $varName[${wE + wF - 1}:$wF] == ${(1 << wE) - 1} ? 11'd2047 : 11'd${(1 << 10) - (1 << (wE - 1))} + $varName[${wE + wF - 1}:$wF], $varName[${wF - 1}:0]${if wF<52 then s", ${52 - wF}'d0" else ""}}) - $$bitstoreal(64'h${java.lang.Long.toHexString(java.lang.Double.doubleToLongBits(value))}); epsilon = $$bitstoreal(64'h${java.lang.Long.toHexString(java.lang.Double.doubleToLongBits(epsilon))}); $$display(\"tmpr: %0f epsilon: %0f\",tmpr, epsilon); if(tmpr > epsilon || -tmpr > epsilon) $$finish;"
    case _ => throw Exception(s"Unsupported type $hw for testbench.")
}
