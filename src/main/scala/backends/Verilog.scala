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
  /** Name of the generated module (-module). Was always "main", which the users of a design renamed by hand: a design
   *  regenerated from the command line recorded in its header then came out under the wrong name. */
  var moduleName: String = "main"

  extension (mod:Module)
    /**
     * @return a string containing the verilog code of the module
     */
    def toVerilog: String =
      // Get IDs for each regular RTL nodes. An RTL node may use several ids.
      val indexes = HashMap.from(mod.components.zip(mod.components.map {
        case _: Input | _: Output | _: Wire | _: Const => 0
        case Register(_, cycles, _) if cycles > 1 => 2
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

      def width(c: Component) = if c.size != 1 then s"[${c.size - 1}:0] " else ""
      val resetName = mod.inputs.collectFirst { case Input(_, name) if name == "reset" => name }.getOrElse("1'b0")

      // Fan-out replication (-maxfanout N). The last stage of a register estimated to drive more than N loads per bit
      // is emitted as several copies, one per group of consumers holding at most N loads, each marked keep so that
      // neither synthesis nor opt_design merges them back into one driver. Loads are counted through the wire-only
      // nodes (taps, concatenations, feedback wires) down to the consumers that become cells: a mux select drives one
      // LUT per bit of the mux, a RAM address every primitive of the RAM, anything else one cell. The write enable
      // of the RAMs, otherwise one register for all of them, gets a register per RAM instance.
      //
      // A RAM node is emitted as one or more xpm instances ("pieces"): its block part, and its distributed part (the
      // remainder of -ramsplit, or the whole of a shallow RAM) in slices narrow enough that a slice's address fan-out
      // (one RAMD64E per bit per 64 words, plus the F7/F8 read muxes and the write decode of a deeper RAM) fits N.
      // Each piece's write and read address ports are consumers of their own, so that a deep distributed remainder,
      // or a register addressing both ports of one RAM, does not keep hundreds of loads on one address copy. A
      // consumer is therefore (node, port), the port being 0 for anything but a RAM address and 2 * piece + 1 (write)
      // or 2 * piece + 2 (read) for those.
      type Consumer = (Component, Int)
      case class Piece(index: Int, lo: Int, hi: Int, block: Boolean, name: String, dout: String)
      def pieces(cur: RAM): Seq[Piece] =
        val depth = 1 << cur.wr.size
        val base = getName(cur, 1)
        val out = getName(cur)
        val blockPart: Option[(Int, Int)] =
          if depth < RAM.blockDepth then None
          else RAM.splitWidth match
            case Some(w) if cur.size > w => Some((0, w))
            case _ => Some((0, cur.size))
        val distLo = blockPart.map(_._2).getOrElse(0)
        val distWidth = cur.size - distLo
        val sliceBits = Register.maxFanout.map(n => math.max(1, n / distLoadsPerBit(depth))).getOrElse(math.max(1, distWidth))
        val nSlices = if distWidth == 0 then 0 else (distWidth + sliceBits - 1) / sliceBits
        val block = blockPart.map((lo, hi) => Piece(0, lo, hi, true, base, if nSlices == 0 then out else s"${out}_lo")).toSeq
        val dist = (0 until nSlices).map { k =>
          val lo = distLo + k * sliceBits
          val hi = math.min(cur.size, lo + sliceBits)
          val suffix = if blockPart.isDefined then (if nSlices == 1 then "_hi" else s"_hi$k") else (if nSlices == 1 then "" else s"_s$k")
          Piece(block.size + k, lo, hi, false, s"$base$suffix", if suffix.isEmpty then out else s"$out$suffix")
        }
        block ++ dist
      def distLoadsPerBit(depth: Int) = math.max(1, depth / 64 + depth / 128 + depth / 256)
      def pieceLoads(cur: RAM, pc: Piece): Int =
        if pc.block then math.max(1, (pc.hi - pc.lo + 71) / 72) else (pc.hi - pc.lo) * distLoadsPerBit(1 << cur.wr.size)
      def ramInstances(cur: RAM): Seq[String] = pieces(cur).map(_.name)
      val fid = mod.components.zipWithIndex.toMap
      def cid(f: Consumer) = if f._2 == 0 then s"${fid(f._1)}" else s"${fid(f._1)}p${f._2}"
      def transparent(c: Component) = c match
        case _: Tap | _: Concat | _: Wire => true
        case _ => false
      val users: Map[Component, Seq[Component]] = mod.components.flatMap(c => c.parents.map(p => (p, c))).groupMap(_._1)(_._2).withDefaultValue(Seq())
      def loads(f: Component, x: Component): Int = f match
        // Two loads per bit: measured on the routed N=4096 SSR-8 cores (a 44-bit 2:1 mux put 86 loads on its select
        // copy). With the 48-load groups this also gives every data switch a copy of its own, which places better than
        // a copy shared between two switches: the copy has one datapath to sit next to.
        case Mux(address, _) if x == address => 2 * f.size
        case BlockROM(_, address, _) if x == address => 2
        case _ => 1
      def finals(start: Component): Seq[(Consumer, Int)] =
        val acc = collection.mutable.ArrayBuffer[(Consumer, Int)]()
        val seen = collection.mutable.HashSet[Component]()
        def visit(p: Component): Unit = for c <- users(p) do
          if transparent(c) then { if seen.add(c) then visit(c) }
          else c match
            case r: RAM if p == r.wr || p == r.rd =>
              for pc <- pieces(r) do
                if p == r.wr then acc += (((c, 2 * pc.index + 1), pieceLoads(r, pc)))
                if p == r.rd then acc += (((c, 2 * pc.index + 2), pieceLoads(r, pc)))
            case _ => acc += (((c, 0), loads(c, p)))
        visit(start)
        acc.toSeq
      // Replicated registers: consumer -> copy index, first fit into copies of at most maxFanout loads (a consumer
      // heavier than that gets a copy of its own)
      val replicated: Map[Component, Map[Consumer, Int]] = Register.maxFanout match
        case None => Map()
        case Some(n) => mod.components.collect { case r: Register =>
          val fs = finals(r).groupMapReduce(_._1)(_._2)(_ + _).toSeq.sortBy(-_._2)
          val groups = collection.mutable.ArrayBuffer[Int]()
          val assignment = fs.map { (f, l) =>
            val g = groups.indexWhere(_ + l <= n)
            if g >= 0 then { groups(g) += l; (f, g) } else { groups += l; (f, groups.size - 1) }
          }
          if groups.size > 1 then Some((r: Component) -> assignment.toMap) else None
        }.flatten.toMap
      def copies(r: Component) = replicated.get(r).map(_.values.max + 1).getOrElse(0)
      // Wire-only nodes downstream of a replicated register get one copy per consumer, so that each consumer's path
      // leads to its own register copy
      val splitMemo = collection.mutable.HashMap[Component, Boolean]()
      def split(c: Component): Boolean = splitMemo.get(c) match
        case Some(b) => b
        case None =>
          val b = transparent(c) && c.parents.exists(p => replicated.contains(p) || split(p))
          splitMemo(c) = b
          b
      def splitEmitted(c: Component) = split(c) && !c.isInstanceOf[Wire]
      // Name of parent p as seen from consumer f: its copy for f when f is a consumer that becomes cells, the
      // original name from a wire-only node (whose own copies are emitted separately, see splitAssignments)
      def refp(p: Component, f: Consumer): String = p match
        case Wire(input) => refp(input, f)
        case _ if transparent(f._1) => getName(p)
        case _ if replicated.contains(p) => replicated(p).get(f).map(g => s"${getName(p)}_r$g").getOrElse(getName(p))
        case _ if split(p) => s"${getName(p)}_v${cid(f)}"
        case _ => getName(p)
      def ref(p: Component, f: Component): String = refp(p, (f, 0))
      def cast(operand: String, signed: Boolean) = if signed then s"$$signed($operand)" else operand
      def copySource(r: Register) = r match
        case Register(input, 1, _) => ref(input, r)
        case Register(_, 2, _) => getName(r, 1)
        case Register(_, cycles, _) => s"${getName(r, 1)} [${cycles - 2}]"
      def copyUpdates(r: Register) = (0 until copies(r)).map(g => s"${getName(r)}_r$g <= ${copySource(r)};")
      def weName(inst: String) = if Register.maxFanout.isDefined then s"we_$inst" else "ram_we"
      val copyDeclarations = mod.components.flatMap {
        case cur: Register if replicated.contains(cur) => (0 until copies(cur)).map(g => s"(* keep = \"true\" *) reg ${width(cur)}${getName(cur)}_r$g;")
        case cur if splitEmitted(cur) => finals(cur).map(_._1).distinct.map(f => s"wire ${width(cur)}${getName(cur)}_v${cid(f)};")
        case cur: RAM if Register.maxFanout.isDefined => ramInstances(cur).map(i => s"(* keep = \"true\" *) reg ${weName(i)} = 1'b0; // write enable of this RAM (see below)")
        case _ => Seq()
      }
      val copyInitial = mod.components.collect { case r: Register if replicated.contains(r) => (0 until copies(r)).map(g => s"${getName(r)}_r$g = 0;") }.flatten
      val splitAssignments = mod.components.filter(splitEmitted).flatMap(t => finals(t).map(_._1).distinct.map { f =>
        val rhs = t match
          case Tap(input, range) => s"${refp(input, f)}[${if (range.size > 1) s"${range.last}:" else ""}${range.start}]"
          case Concat(inputs) => inputs.map(refp(_, f)).mkString("{", ", ", "}")
          case _ => throw Exception(s"Unexpected wire-only node $t")
        s"  assign ${getName(t)}_v${cid(f)} = $rhs;\n"
      })
      val weUpdates = if Register.maxFanout.isDefined then mod.components.collect { case cur: RAM => ramInstances(cur).map(i => s"${weName(i)} <= ~$resetName;") }.flatten else Seq(s"ram_we <= ~$resetName;")

      val declarations = (mod.components.flatMap {
        case _: Output | _: Input | _: Const | _: Wire => Seq()
        // ROMs small enough not to be block ROMs (see BlockROM) are kept in distributed memory, whatever the synthesizer would infer.
        case cur@Mux(address, inputs) if address.size > 1 => Seq(s"${if inputs.forall(_.isInstanceOf[Const]) then "(* rom_style = \"distributed\" *) " else ""}reg ${if (cur.size != 1) s"[${cur.size - 1}:0] " else ""}${getName(cur)};")
        case cur@Register(_, cycles, keep) if cycles == 1 => Seq(s"${if keep then "(* keep = \"true\" *) " else ""}reg ${if (cur.size != 1) s"[${cur.size - 1}:0] " else ""}${getName(cur)};")
        case cur@Register(_, cycles, _) if cycles == 2 => Seq(
          s"reg ${if (cur.size != 1) s"[${cur.size - 1}:0] " else ""}${getName(cur, 1)};",
          s"reg ${if (cur.size != 1) s"[${cur.size - 1}:0] " else ""}${getName(cur)};")
        case cur@Register(_, cycles, _) => Seq(
          s"reg ${if (cur.size != 1) s"[${cur.size - 1}:0] " else ""}${getName(cur, 1)} [${cycles - 1}:0];",
          s"wire ${if (cur.size != 1) s"[${cur.size - 1}:0] " else ""}${getName(cur)};")
        case cur: RAM => Seq(s"wire ${if (cur.size != 1) s"[${cur.size - 1}:0] " else ""}${getName(cur)};")
        case cur: Dsp48 => Seq(s"wire [47:0] ${getName(cur)}; wire [47:0] ${getName(cur)}_pc;")
        case cur: BlockROM => Seq(s"wire ${if (cur.size != 1) s"[${cur.size - 1}:0] " else ""}${getName(cur)};")
        case cur => Seq(s"wire ${if (cur.size != 1) s"[${cur.size - 1}:0] " else ""}${getName(cur)};")
      } ++ copyDeclarations :+ "integer i;" :+ (if Register.maxFanout.isDefined then "// write enables of the RAMs: one register per instance (see above)" else "reg ram_we = 1'b0; // write enable of the RAMs (see below)")).map(s => s"  $s\n").mkString("")

      // Power-up values of the registers (zero, as on an FPGA), so that a simulation starts in the state the hardware starts in: reset
      // does not clear the control token chains, which would otherwise stay undefined for as long as the latency of the design. Written
      // as an initial block after the combinational blocks rather than as declaration initialisers, so that the first evaluation of
      // the always @(*) blocks sees them (they only run on an event, and a declaration initialiser is not one).
      val initial = (mod.components.flatMap {
        case cur@Register(_, cycles, _) if cycles == 1 => Seq(s"${getName(cur)} = 0;")
        case cur@Register(_, cycles, _) if cycles == 2 => Seq(s"${getName(cur, 1)} = 0;", s"${getName(cur)} = 0;")
        case cur@Register(_, cycles, _) => Seq(s"for (i = 0; i < $cycles; i = i + 1) ${getName(cur, 1)}[i] = 0;")
        case _ => Seq()
      } ++ copyInitial).map(s => s"      $s\n").mkString("")
      val assignments = (mod.components.flatMap(cur => (cur match
        case Output(input, _) => Some(ref(input, cur))
        case Plus(terms, signed) => Some(terms.map(t => cast(ref(t, cur), signed)).mkString(" + "))
        case Minus(lhs, rhs, signed) => Some(s"${cast(ref(lhs, cur), signed)} - ${cast(ref(rhs, cur), signed)}")
        case Times(lhs, rhs) => Some(s"$$signed(${ref(lhs, cur)}) * $$signed(${ref(rhs, cur)})")
        case And(terms) => Some(terms.map(ref(_, cur)).mkString(" & "))
        case Xor(inputs) => Some(inputs.map(ref(_, cur)).mkString(" ^ "))
        case Or(inputs) => Some(inputs.map(ref(_, cur)).mkString(" | "))
        case Equals(lhs, rhs) => Some(s"${ref(lhs, cur)} == ${ref(rhs, cur)}")
        case Not(input) => Some(s"~${ref(input, cur)}")
        case Concat(inputs) => Some(inputs.map(ref(_, cur)).mkString("{",", ","}"))
        case Tap(input, range) => Some(s"${ref(input, cur)}[${if (range.size > 1) s"${range.last}:" else ""}${range.start}]")
        case Register(input, cycles, _) if cycles > 2 => Some(s"${getName(cur,1)} [${cycles - 1}]")
        case Mux(address, inputs) if address.size == 1 => Some(s"${ref(address, cur)} ? ${ref(inputs.last, cur)} : ${ref(inputs.head, cur)}")
        case _ => None
      ).map((cur, _))).map((cur, rhs) => s"  assign ${getName(cur)} = $rhs;\n") ++ splitAssignments).mkString("")

      val sequential = (weUpdates ++ mod.components.flatMap {
        case cur@Register(input, cycles, _) if cycles == 1 => s"${getName(cur)} <= ${ref(input, cur)};" +: copyUpdates(cur)
        case cur@Register(input, cycles, _) if cycles == 2 => Seq(
          s"${getName(cur, 1)} <= ${ref(input, cur)};",
          s"${getName(cur)} <= ${getName(cur, 1)};") ++ copyUpdates(cur)
        case cur@Register(input, cycles, _) => Seq(
          s"${getName(cur, 1)} [0] <= ${ref(input, cur)};",
          s"for (i = 1; i < $cycles; i = i + 1)",
          s"  ${getName(cur, 1)} [i] <= ${getName(cur, 1)} [i - 1];") ++ copyUpdates(cur)
        case _ => Seq()
      }).map(s => s"      $s\n").mkString("")

      val combinatorial = mod.components.flatMap {
        case cur@Mux(address, inputs) if address.size > 1 =>
          "always @(*)" +:
            s"  case(${ref(address, cur)})" +:
            inputs.zipWithIndex.map((in, i) => s"    ${if (i == inputs.size - 1 && ((1 << address.size) != inputs.size)) "default" else i}: ${getName(cur)} = ${ref(in, cur)};") :+
            "  endcase"
        case cur@RAM(data, wr, rd) =>
          // Block RAMs use NO_CHANGE mode, which supports a higher clock frequency than READ_FIRST on UltraScale devices, but requires that
          // the two ports never access the same address in the same cycle (true for the permutations generated by SGen, which read an
          // address before rewriting it). Vivado maps an XPM RAM whose write enable is a constant to READ_FIRST whatever the write mode
          // says, hence the registered enable. Distributed RAMs only support READ_FIRST.
          // A block RAM word wider than a RAMB18's 36 bits (or a RAMB36's 72) takes the next primitive for a few bits: keep the
          // first splitWidth bits in block RAM and the remainder in distributed RAM at the same addresses (-ramsplit); see pieces.
          val ps = pieces(cur)
          def xpm(pc: Piece) = Seq(
            "xpm_memory_sdpram #(",
            s"  .ADDR_WIDTH_A(${wr.size}), .ADDR_WIDTH_B(${rd.size}), .WRITE_DATA_WIDTH_A(${pc.hi - pc.lo}), .BYTE_WRITE_WIDTH_A(${pc.hi - pc.lo}), .READ_DATA_WIDTH_B(${pc.hi - pc.lo}),",
            s"  .MEMORY_SIZE(${(pc.hi - pc.lo) << wr.size}), .MEMORY_PRIMITIVE(\"${if pc.block then "block" else "distributed"}\"), .CLOCKING_MODE(\"common_clock\"),",
            s"  .READ_LATENCY_B(${RAM.readLatency}), .WRITE_MODE_B(\"${if pc.block then "no_change" else "read_first"}\"), .SIM_ASSERT_CHK(0)",
            s") ${pc.name} (",
            s"  .clka(clk), .ena(1'b1), .wea(${weName(pc.name)}), .addra(${refp(wr, (cur, 2 * pc.index + 1))}), .dina(${ref(data, cur)}[${pc.hi - 1}:${pc.lo}]),",
            s"  .clkb(clk), .enb(1'b1), .regceb(1'b1), .rstb(1'b0), .addrb(${refp(rd, (cur, 2 * pc.index + 2))}), .doutb(${pc.dout}),",
            "  .sleep(1'b0), .injectsbiterra(1'b0), .injectdbiterra(1'b0), .sbiterrb(), .dbiterrb());")
          val wires = if ps.size > 1 then Seq(ps.map(pc => s"wire [${pc.hi - pc.lo - 1}:0] ${pc.dout};").mkString(" ") + s" assign ${getName(cur)} = {${ps.reverse.map(_.dout).mkString(", ")}};") else Seq()
          wires ++ ps.flatMap(xpm)
        case cur@BlockROM(values, address, _) =>
          val cfg = BlockRAMConfig(values.size, cur.size).get
          blockMemory(getName(cur), getName(cur, 1), cfg, cur.size, address.size, ref(address, cur), None, Some(values))
        case cur: Dsp48 =>
          // One DSP48E2 per block, every register setting explicit (see ir.rtl.Dsp48 and FixedPoint.FixDsp). Inferring
          // the blocks from arithmetic left the mapping to the synthesizer, which absorbed or declined the operand
          // registers case by case, and to the physical optimiser, which moved them.
          def sext(comp: Component, w: Int): String = comp match
            case Const(size, value) =>
              val v = if size > 0 && value.testBit(size - 1) then value - (BigInt(1) << size) else value
              s"$w'd${v & ((BigInt(1) << w) - 1)}"
            case _ if comp.size >= w => s"${ref(comp, cur)}[${w - 1}:0]"
            case _ => s"{{${w - comp.size}{${ref(comp, cur)}[${comp.size - 1}]}}, ${ref(comp, cur)}}"
          val pre = cur.preAdd
          val breg = cur.breg
          val aStr = sext(cur.a, 30)
          val dStr = cur.d.map(sext(_, 27)).getOrElse("27'b0")
          val bStr = cur.b match
            case Left(comp) => sext(comp, 18)
            case Right(value) => s"18'd${value & 0x3ffff}"
          val cStr = cur.c.map {
            case Const(size, value) =>
              val v = if size > 0 && value.testBit(size - 1) then value - (BigInt(1) << size) else value
              s"48'd${(v << cur.cShift) & ((BigInt(1) << 48) - 1)}"
            case comp =>
              val ext = 48 - comp.size - cur.cShift
              val parts = (if ext > 0 then Seq(s"{$ext{${ref(comp, cur)}[${comp.size - 1}]}}") else Seq()) ++ Seq(ref(comp, cur)) ++ (if cur.cShift > 0 then Seq(s"${cur.cShift}'b0") else Seq())
              if parts.size == 1 then parts.head else parts.mkString("{", ", ", "}")
          }.getOrElse("48'b0")
          val opmode = s"9'b${if cur.rnd != 0 then "10" else "00"}_${if cur.pcin.isDefined then "001" else if cur.c.isDefined then "011" else "000"}_01_01"
          val alumode = (cur.negateZ, cur.negateM) match
            case (false, false) => "0000"
            case (false, true) => "0011"
            case (true, false) => "0001"
            case (true, true) => "0010"
          val inmode = if pre then (if cur.subtractA then "5'b01100" else "5'b00100") else "5'b00000"
          Seq(
            "(* dont_touch = \"true\" *) DSP48E2 #(",
            s"  .AMULTSEL(\"${if pre then "AD" else "A"}\"), .A_INPUT(\"DIRECT\"), .BMULTSEL(\"B\"), .B_INPUT(\"DIRECT\"), .PREADDINSEL(\"A\"), .RND(48'd${cur.rnd}), .USE_MULT(\"MULTIPLY\"), .USE_SIMD(\"ONE48\"), .USE_WIDEXOR(\"FALSE\"), .XORSIMD(\"XOR24_48_96\"),",
            "  .AUTORESET_PATDET(\"NO_RESET\"), .AUTORESET_PRIORITY(\"RESET\"), .MASK(48'h3fffffffffff), .PATTERN(48'h000000000000), .SEL_MASK(\"MASK\"), .SEL_PATTERN(\"PATTERN\"), .USE_PATTERN_DETECT(\"NO_PATDET\"),",
            s"  .ACASCREG(1), .ADREG(${if pre then 1 else 0}), .ALUMODEREG(0), .AREG(1), .BCASCREG($breg), .BREG($breg), .CARRYINREG(0), .CARRYINSELREG(0), .CREG(${if cur.c.isDefined then 1 else 0}), .DREG(${if pre then 1 else 0}), .INMODEREG(0), .MREG(1), .OPMODEREG(0), .PREG(1)",
            s") ${getName(cur)}_dsp (",
            s"  .P(${getName(cur)}), .PCOUT(${getName(cur)}_pc), .ACOUT(), .BCOUT(), .CARRYCASCOUT(), .CARRYOUT(), .MULTSIGNOUT(), .OVERFLOW(), .PATTERNBDETECT(), .PATTERNDETECT(), .UNDERFLOW(), .XOROUT(),",
            s"  .A($aStr), .B($bStr), .C($cStr), .D($dStr), .PCIN(${cur.pcin.map(p => s"${getName(p)}_pc").getOrElse("48'b0")}), .ACIN(30'b0), .BCIN(18'b0), .CARRYCASCIN(1'b0), .MULTSIGNIN(1'b0), .CARRYIN(1'b${if cur.negateZ then 1 else 0}),",
            s"  .ALUMODE(4'b$alumode), .CARRYINSEL(3'b000), .INMODE($inmode), .OPMODE($opmode), .CLK(clk),",
            s"  .CEA1(1'b0), .CEA2(1'b1), .CEAD(1'b${if pre then 1 else 0}), .CEALUMODE(1'b0), .CEB1(1'b${if breg == 2 then 1 else 0}), .CEB2(1'b${if breg >= 1 then 1 else 0}), .CEC(1'b${if cur.c.isDefined then 1 else 0}), .CECARRYIN(1'b0), .CECTRL(1'b0), .CED(1'b${if pre then 1 else 0}), .CEINMODE(1'b0), .CEM(1'b1), .CEP(1'b1),",
            "  .RSTA(1'b0), .RSTALLCARRYIN(1'b0), .RSTALUMODE(1'b0), .RSTB(1'b0), .RSTC(1'b0), .RSTCTRL(1'b0), .RSTD(1'b0), .RSTINMODE(1'b0), .RSTM(1'b0), .RSTP(1'b0));")
        case cur: Extern =>
          mod.dependencies.add(cur.filename)
          Seq(s"${cur.module} ext_${getName(cur)}(${cur.inputs.map { case (name, comp) => s".$name(${ref(comp, cur)}), " }.mkString}.${cur.outputName}(${getName(cur)}));")
        case _ => Seq()
      }.map(s => s"  $s\n").mkString("")

      val result = new StringBuilder
      result ++= "`timescale 1ns/1ps // the design is clock-period agnostic; a directive lets it elaborate alongside modules that have one (XPM, unisim)\n"
      result ++= s"module ${Verilog.moduleName}(input clk,\n"
      result ++= mod.inputs.map(s => s"  input ${if (s.size != 1) s"[${s.size - 1}:0] " else ""}${getName(s)},\n").mkString("")
      result ++= mod.outputs.map(s => s"  output ${if (s.size != 1) s"[${s.size - 1}:0] " else ""}${getName(s)}").mkString(",\n")
      result ++= ");\n\n"
      result ++= declarations
      result ++= assignments
      result ++= combinatorial
      if initial.nonEmpty then
        result ++= "  initial\n    begin\n"
        result ++= initial
        result ++= "    end\n"
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
      // The design's inputs are driven from time 0: left undefined until the first clock edge, `next` would enter the
      // design's registered input as X, and the 0 -> X transition it causes on next_out satisfies @(posedge next_out)
      // below, so that the outputs would be sampled before any dataset has entered the design.
      res ++= "    reg clk = 0, rst = 0, next = 0;\n"
      sm.dataInputs.foreach(res ++= "    reg [" ++= (sm.hw.size - 1).toString ++= ":0] " ++= _.name ++= " = 0;\n")
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
      res ++= s"      ${Verilog.moduleName} uut(clk,rst,next," ++= (0 until sm.K).map(i => sm.dataInputs(i).name).mkString(",") ++= ",next_out," ++= (0 until sm.K).map(i => sm.dataOutputs(i).name).mkString(",") ++= ");\n"
      res ++= "endmodule\n"
      res.toString
    }
  }

  private def displayInVerilog(hw: HW[?], varName: String): String = hw match
    case ComplexHW(hw) if !hw.isInstanceOf[ComplexHW[?]] => s"tmp=$varName[${hw.size - 1}:0]; ${displayInVerilog(hw, "tmp")} $$write(\" + \"); tmp=$varName[${2 * hw.size - 1}:${hw.size}]; ${displayInVerilog(hw, "tmp")} $$write(\"i\");"
    case FixedPoint(_, fractional, _) => s"$$write(\"%0f\",$$itor($$signed($varName))/${1 << fractional});"
    case Flopoco(wE, wF) if wE <= 11 && wF <= 52 => s"if($varName[${wE+wF}]) $$write(\"-\"); if($varName[${wE+wF+2}:${wE+wF+1}] == 0) $$write(\"0\"); else if($varName[${wE+wF+2}:${wE+wF+1}] == 2) $$write(\"infinity\"); else if($varName[${wE+wF+2}:${wE+wF+1}] == 3) $$write(\"NaN\"); else $$write(\"%0f\", $$bitstoreal({1'd0,11'd${(1<<10)-(1<<(wE-1))}+$varName[${wE+wF-1}:$wF],$varName[${wF-1}:0]${if wF<52 then s",${52-wF}'d0" else ""}}));"
    case IEEE754(wE, wF) if wE <= 11 && wF <= 52 => s"$$write(\"%0f\", $$bitstoreal({$varName[${wE+wF}],$varName[${wE+wF-1}:$wF] == 0 ? 11'd0 : $varName[${wE+wF-1}:$wF] == ${(1<<wE) - 1} ? 11'd2047 : 11'd${(1<<10)-(1<<(wE-1))} + $varName[${wE+wF-1}:$wF], $varName[${wF-1}:0]${if wF<52 then s", ${52-wF}'d0" else ""}}));"
    case _ => s"$$write(\"%0d\", $varName);"

  private def callFinishIfDifferent[T](hw:HW[T], varName: String, value: T, epsilon: Double): String = hw match
    case Flopoco(wE, wF) if wE <= 11 && wF <= 52 => s"tmpr = $varName[${wE+wF+2}:${wE+wF+1}] == 0?0:$$bitstoreal({$varName[${wE+wF}],11'd${(1<<10)-(1<<(wE-1))}+$varName[${wE+wF-1}:$wF],$varName[${wF-1}:0]${if wF<52 then s",${52-wF}'d0" else ""}}) - $$bitstoreal(64'h${java.lang.Long.toHexString(java.lang.Double.doubleToLongBits(value))}); epsilon = $$bitstoreal(64'h${java.lang.Long.toHexString(java.lang.Double.doubleToLongBits(epsilon))}); $$display(\"tmpr: %0f epsilon: %0f\",tmpr, epsilon); if(tmpr > epsilon || -tmpr > epsilon) $$finish;"
    case FixedPoint(magnitude, fractional, _) =>  s"if(($$signed($varName) > ${hw.size}'sd${hw.bitsOf(value)} ? $$signed($varName) - ${hw.size}'sd${hw.bitsOf(value)} : ${hw.size}'sd${hw.bitsOf(value)} - $$signed($varName)) > ${hw.size}'sd${hw.bitsOf(epsilon)}) $$finish;"
    case ComplexHW(hw) => s"tmp = $varName[${hw.size - 1}:0]; ${callFinishIfDifferent(hw,"tmp", value.re, epsilon)} tmp = $varName[${2 * hw.size - 1}:${hw.size}]; ${callFinishIfDifferent(hw,"tmp", value.im, epsilon)}"
    case Unsigned(size) => s"if(($varName > $size'd${hw.bitsOf(value)} ? $varName - $size'd${hw.bitsOf(value)} : $size'd${hw.bitsOf(value)} - $varName) > $size'd${hw.bitsOf(epsilon.toInt)}) $$finish;"
    case IEEE754(wE, wF) if wE <= 11 && wF <= 52 => s"tmpr = $$bitstoreal({$varName[${wE + wF}],$varName[${wE + wF - 1}:$wF] == 0 ? 11'd0 : $varName[${wE + wF - 1}:$wF] == ${(1 << wE) - 1} ? 11'd2047 : 11'd${(1 << 10) - (1 << (wE - 1))} + $varName[${wE + wF - 1}:$wF], $varName[${wF - 1}:0]${if wF<52 then s", ${52 - wF}'d0" else ""}}) - $$bitstoreal(64'h${java.lang.Long.toHexString(java.lang.Double.doubleToLongBits(value))}); epsilon = $$bitstoreal(64'h${java.lang.Long.toHexString(java.lang.Double.doubleToLongBits(epsilon))}); $$display(\"tmpr: %0f epsilon: %0f\",tmpr, epsilon); if(tmpr > epsilon || -tmpr > epsilon) $$finish;"
    case _ => throw Exception(s"Unsupported type $hw for testbench.")
}
