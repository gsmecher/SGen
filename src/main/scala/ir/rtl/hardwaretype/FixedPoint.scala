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

package ir.rtl.hardwaretype

import Utils.BigIterator
import ir.rtl.Component
import ir.rtl.signals.{Const, Minus, Plus, Sig, Times, Zero}

/**
 * Fixed point arithmetic representation
 *
 * @param magnitude Number of bits of the integer part
 * @param fractional Number of bits of the fractional part
 */
/** Signed fixed-point numbers with `magnitude` integer bits (sign included) and `fractional` fractional bits. With
 *  `saturating`, constants beyond the range are clamped instead of rejected: meant for twiddles, whose only out-of-range
 *  value is 1.0 itself (stored as 1 - 2^-fractional; the trivial-multiplier rules still recognise it as one). */
case class FixedPoint(magnitude: Int, fractional: Int, saturating: Boolean = false) extends HW[Double](magnitude + fractional):
  // ------------------------------------------------------------------------------------------------------------------
  // Arithmetic. Sums and differences are fabric adders (FixPlus, FixMinus, FixAddShift); products and everything that
  // can ride along with a product in a DSP48E2 block are one node, FixDsp, a chain of blocks with slots for a pre-added
  // operand, an added operand on C, the rounding constant on W, and the cascade between blocks. The rules below fill
  // the slots: a sum absorbs into a neighbouring block with a free input rather than becoming an adder.
  // ------------------------------------------------------------------------------------------------------------------

  /** The multiplicand fits the block's 18-bit port and a pre-added sum of two operands its 27-bit one */
  private def preOk(w: Sig[Double]): Boolean = w.hw.size <= 18 && size + 1 <= 27

  /** Two plain products by the same constant, (a c) and (b c): one block computes (a +- b) c in its pre-adder */
  private def preAddable(l: FixDsp, r: FixDsp): Boolean = (l.single, r.single) match
    case (Some(bl), Some(br)) => bl.d.isEmpty && br.d.isEmpty && !bl.negate && !br.negate && bl.preShift == 0 && br.preShift == 0 &&
      ((bl.b, br.b) match
        case (cl: Const[?], cr: Const[?]) => cl == cr && preOk(bl.b)
        case _ => false)
    case _ => false

  /** Two chains that can share one cascade: the same scaling throughout, at most one C operand, no halving yet, four blocks at most */
  private def chainable(l: FixDsp, r: FixDsp): Boolean =
    l.shift == 0 && r.shift == 0 && l.blocks.size + r.blocks.size <= 4 && (l.c.isEmpty || r.c.isEmpty) &&
      (l.blocks ++ r.blocks).forall(b => b.b.hw == l.blocks.head.b.hw && b.preShift == l.blocks.head.preShift)

  /** l +- r as one chain: the chain with the C operand goes first (C is an input of the first block only) */
  private def chain(l: FixDsp, r: FixDsp, subtract: Boolean): FixDsp =
    val rr = if subtract then r.negated else r
    if l.c.isDefined || rr.c.isEmpty then FixDsp(l.blocks ++ rr.blocks, l.c, l.negateC, 0) else FixDsp(rr.blocks ++ l.blocks, rr.c, rr.negateC, 0)

  override def plus(lhs: Sig[Double], rhs: Sig[Double]): Sig[Double] = (lhs, rhs) match
    case (l: FixDsp, r: FixDsp) if preAddable(l, r) =>
      val (bl, br) = (l.blocks.head, r.blocks.head)
      FixDsp(Seq(Blk(bl.a, Some(br.a), subtractA = false, bl.b, 0, negate = false)), None, false, 0)
    case (l: FixDsp, r: FixDsp) if chainable(l, r) => chain(l, r, subtract = false)
    case (l: FixDsp, d) if l.c.isEmpty && l.shift == 0 => l.copy(c = Some(d), negateC = false)
    case (d, r: FixDsp) if r.c.isEmpty && r.shift == 0 => r.copy(c = Some(d), negateC = false)
    case _ => FixPlus(lhs, rhs)

  override def minus(lhs: Sig[Double], rhs: Sig[Double]): Sig[Double] = (lhs, rhs) match
    // The opposite of a chain (Times writes a product by a negative constant as the opposite of the product by its
    // absolute value): the signs flip inside the blocks, the C input stays free for a butterfly
    case (Zero(), r: FixDsp) => r.negated
    case (l: FixDsp, r: FixDsp) if preAddable(l, r) =>
      val (bl, br) = (l.blocks.head, r.blocks.head)
      FixDsp(Seq(Blk(br.a, Some(bl.a), subtractA = true, bl.b, 0, negate = false)), None, false, 0) // (bl.a - br.a) c: D = bl.a, A = br.a
    case (l: FixDsp, r: FixDsp) if chainable(l, r) => chain(l, r, subtract = true)
    case (l: FixDsp, d) if l.c.isEmpty && l.shift == 0 => l.copy(c = Some(d), negateC = true)
    case (d, r: FixDsp) if r.c.isEmpty && r.shift == 0 => r.negated.copy(c = Some(d), negateC = false)
    case _ => FixMinus(lhs, rhs)

  override def times(lhs: Sig[Double], rhs: Sig[Double]): Sig[Double] = (lhs, rhs) match
    // A chain scaled by a negative power of two (a scaled butterfly on a block's C input, or on a cascade, possibly
    // scaled again): the scaling folds into the chain's output slice, the sum being exact in the block and the rounding
    // constant following the slice
    case (p: FixDsp, Const(value)) if rightShift(value, rhs.hw) > 0 => p.copy(shift = p.shift + rightShift(value, rhs.hw))
    // A sum or difference scaled by a negative power of two (as in a scaled butterfly): computed with the extra bits so that it never overflows.
    case (p: FixPlus, Const(value)) if rightShift(value, rhs.hw) > 0 => FixAddShift(p.lhs, p.rhs, subtract = false, rightShift(value, rhs.hw))
    case (p: FixMinus, Const(value)) if rightShift(value, rhs.hw) > 0 => FixAddShift(p.lhs, p.rhs, subtract = true, rightShift(value, rhs.hw))
    // A positive power of two: a wire
    case (_, Const(value)) if value > 0 && rhs.hw.bitsOf(value).bitCount == 1 => FixTimes(lhs, rhs)
    // A sum or difference (possibly halved or quartered) times a factor the 18-bit port takes: the sum is the block's pre-adder's
    case (p: FixPlus, w) if preOk(w) => FixDsp(Seq(Blk(p.lhs, Some(p.rhs), subtractA = false, w, 0, negate = false)), None, false, 0)
    case (p: FixMinus, w) if preOk(w) => FixDsp(Seq(Blk(p.rhs, Some(p.lhs), subtractA = true, w, 0, negate = false)), None, false, 0)
    case (p: FixAddShift, w) if preOk(w) && p.shift <= 2 => FixDsp(Seq(Blk(p.rhs, Some(p.lhs), subtractA = p.subtract, w, p.shift, negate = false)), None, false, 0)
    case _ => FixDsp(Seq(Blk(lhs, None, subtractA = false, rhs, 0, negate = false)), None, false, 0)

  /** Number of bits of right shift implementing a multiplication by a constant that is a negative power of two, 0 otherwise */
  private def rightShift(value: Double, hw: HW[Double]): Int =
    val bits = hw.bitsOf(value)
    if value > 0 && bits.bitCount == 1 && bits.lowestSetBit < hw.asInstanceOf[FixedPoint].fractional then hw.asInstanceOf[FixedPoint].fractional - bits.lowestSetBit else 0

  override def bitsOf(const: Double): BigInt = {
    require(const.isFinite)
    if const < 0 then
      val opposite = ((BigInt(1) << fractional).toDouble * BigDecimal(-const)).toBigInt
      if opposite == 0 then
        opposite
      else
        val res = (opposite ^ ((BigInt(1) << size) - 1)) + 1
        if res.bitLength != size then
          if !saturating then throw IllegalArgumentException(s"Overflow during the conversion of ${const} to a ${this}")
          BigInt(1) << (size - 1)
        else
          res
    else
      val res = ((BigInt(1) << fractional).toDouble * BigDecimal(const)).toBigInt
      if res.bitLength >= size then
        if !saturating then throw IllegalArgumentException(s"Overflow during the conversion of ${const} to a ${this}")
        (BigInt(1) << (size - 1)) - 1
      else
        res
  }


  override def valueOf(const: BigInt): Double = {
    require(const.bitLength <= size)
    if const.testBit(size - 1) then
      -((const ^ ((BigInt(1) << size) - 1)) + 1).toDouble / Math.pow(2, fractional)
    else
      const.toDouble / Math.pow(2, fractional)
  }

  override def description: String = if fractional == 0 then s"$magnitude-bits signed integer in two's complement format" else s"signed fixed-point number ($magnitude. $fractional bits representation)"

  // The adders' result registers stay in the fabric (Sig.keepFirst): a sum feeding a DSP block would otherwise be
  // absorbed, register and all, so that its carry chain and the route into the block share a cycle (three CARRY8 leave
  // 0.7 ns of a 1.6 ns cycle for the route on an UltraScale+ device; every adder was a failing class in a four-module
  // design). The block gets its own input register from the product (see FixDsp).
  private case class FixPlus(override val lhs: Sig[Double], override val rhs: Sig[Double]) extends Plus(lhs, rhs):
    override def pipeline = 1
    override def keepFirst = true

    override def implement(implicit cp: Sig[?] => Component) = ir.rtl.Plus(Seq(cp(this.lhs), cp(this.rhs)))

  private case class FixMinus(override val lhs: Sig[Double], override val rhs: Sig[Double]) extends Minus(lhs, rhs):
    override def pipeline = 1
    override def keepFirst = true

    override def implement(implicit cp: Sig[?] => Component) = ir.rtl.Minus(cp(this.lhs), cp(this.rhs))

  /**
   * Whether a signal comes out of a DSP block. The block's output register is the last register on it, and a block
   * consuming it has its input register inside: wired directly, the two blocks sit with nothing in the fabric between
   * their columns (measured: 0.9 ns of route between two DSP columns, failing at 625 MHz). Such an operand gets one more
   * register, kept in the fabric, in front of the block's.
   */
  private def fromDsp(s: Sig[?]): Boolean = s match
    case _: FixDsp => true
    case t: FixTimes => fromDsp(t.lhs) // a power-of-two wire on a block's output is still the block's output
    case _ => false

  private def lead(s: Sig[?]): Int = if fromDsp(s) then 1 else 0

  /** A product by a positive power of two: a wire. Everything else multiplies in a DSP block (FixDsp). */
  private case class FixTimes(override val lhs: Sig[Double], override val rhs: Sig[Double]) extends Times(lhs, rhs):
    override def pipeline = 0
    override def latency = 0

    override def implement(implicit cp: Sig[?] => Component): Component =
      val Const(value) = this.rhs: @unchecked
      val shift = this.rhs.hw.bitsOf(value).lowestSetBit - this.rhs.hw.asInstanceOf[FixedPoint].fractional
      if shift > 0 then
        ir.rtl.Concat(Seq(ir.rtl.Tap(cp(this.lhs), 0 until (this.lhs.hw.size - shift)), ir.rtl.Const(shift, 0)))
      else
        val input = cp(this.lhs)
        val rightShift = -shift
        val sign = ir.rtl.Tap(input, (this.lhs.hw.size - 1) until this.lhs.hw.size)
        ir.rtl.Concat(Seq.fill(rightShift)(sign) :+ ir.rtl.Tap(input, rightShift until this.lhs.hw.size))

  /**
   * Sum or difference of two signals, shifted right by `shift` bits (a scaled butterfly). The sum is computed on `shift` more bits
   * than the operands, so that it cannot overflow, and rounded to nearest.
   *
   * For a shift of one bit, the rounding is convergent (ties to even): a sum that is halved is a tie half of the time, so that rounding
   * ties upwards would bias the result by a quarter of a bit per stage. Whether the result must be incremented only depends on the two
   * low bits of the operands; it is computed one cycle ahead (from the operands one cycle before they enter the adder) and registered,
   * so that it enters the adder as a carry input coming straight from a register (for a difference, as a third operand). For larger
   * shifts, ties are rare and half of the last kept bit is added before the shift.
   */
  private case class FixAddShift(val lhs: Sig[Double], val rhs: Sig[Double], val subtract: Boolean, val shift: Int) extends Sig[Double](using lhs.hw):
    override val hash: Int = Seq("FixAddShift", lhs, rhs, subtract, shift).hashCode()

    override def pipeline = 1
    override def keepFirst = true

    private def convergent = shift == 1

    override def parents: Seq[(Sig[?], Int)] = if convergent then Seq((lhs, 0), (rhs, 0), (lhs, 1), (rhs, 1)) else Seq((lhs, 0), (rhs, 0))

    override def implement(cp: (Sig[?], Int) => Component): Component =
      val (a, b) = (cp(lhs, 0), cp(rhs, 0))
      def extend(c: Component) = ir.rtl.Concat(Seq.fill(shift)(ir.rtl.Tap(c, (size - 1) until size)) :+ c)
      def bit(c: Component, i: Int) = ir.rtl.Tap(c, i until (i + 1))
      val rounded =
        if convergent then
          // The result is odd (a tie) iff a0 != b0; it is then incremented iff bit 1 of the exact sum (a1 ^ b1 ^ carry or borrow from bit 0) is set.
          val (a1, b1) = (cp(lhs, 1), cp(rhs, 1))
          val odd = ir.rtl.Xor(Seq(bit(a1, 0), bit(b1, 0)))
          val carry = if subtract then ir.rtl.And(Seq(ir.rtl.Not(bit(a1, 0)), bit(b1, 0))) else ir.rtl.And(Seq(bit(a1, 0), bit(b1, 0)))
          val increment = ir.rtl.And(Seq(odd, ir.rtl.Xor(Seq(bit(a1, 1), bit(b1, 1), carry)))).register
          if subtract then ir.rtl.Plus(Seq(ir.rtl.Minus(extend(a), extend(b)), increment)) else ir.rtl.Plus(Seq(extend(a), extend(b), increment))
        else
          val sum = if subtract then ir.rtl.Minus(extend(a), extend(b)) else ir.rtl.Plus(Seq(extend(a), extend(b)))
          ir.rtl.Plus(Seq(sum, ir.rtl.Const(size + shift, BigInt(1) << (shift - 1))))
      ir.rtl.Tap(rounded, shift until (shift + size))

  /**
   * One multiplier of a block chain: ((d ± a) or a) × b, scaled by 2^-preShift (the pre-added operands are a halved or
   * quartered butterfly's: the sum is formed exactly and the scaling waits for the output slice), subtracted from the
   * chain when `negate`.
   */
  private case class Blk(a: Sig[Double], d: Option[Sig[Double]], subtractA: Boolean, b: Sig[Double], preShift: Int, negate: Boolean)

  /**
   * A chain of DSP48E2 blocks: ±c ± Σ_i ((d_i ± a_i) × b_i) 2^-preShift, rounded to nearest and truncated to the operands'
   * format (halved once more with `shift`). Each block multiplies in its own multiplier; the first adds the C operand (Z =
   * C) and the rounding constant (W = RND, negative in two's complement when the first product is subtracted, so that the
   * rounding adds half a unit whatever the signs), every next block adds or subtracts its product to the cascade (Z =
   * PCIN). The blocks are instantiated (ir.rtl.Dsp48), registers and all, with dont_touch.
   *
   * Timing, relative to the output: a block's P is one cycle after the P it continues; its operands enter n - i + 2
   * cycles ahead (AREG, MREG, PREG) plus one with the pre-adder (ADREG), the C operand n + 1 (CREG, PREG). An operand
   * that is itself a block's output gets a fabric register in front (lead).
   *
   * The rules in plus, minus and times grow a chain by filling its slots: a sum absorbs into a block with a free C input
   * or pre-adder, two chains with the same scaling join one cascade. A butterfly's two outputs are two chains and compute
   * their product twice; that is the intent (one block per output, no fabric adder), and since a block's parameters are
   * part of its identity the two blocks stay distinct.
   */
  private case class FixDsp(blocks: Seq[Blk], c: Option[Sig[Double]], negateC: Boolean, shift: Int) extends Sig[Double](using blocks.head.a.hw):
    require(blocks.nonEmpty && (negateC == false || c.isDefined))
    override val hash: Int = Seq("FixDsp", blocks, c, negateC, shift).hashCode()

    override def pipeline = 0

    /** The block, when the chain is one plain block without a C operand */
    def single: Option[Blk] = if blocks.size == 1 && c.isEmpty && shift == 0 then Some(blocks.head) else None
    /** The chain subtracted: every product's sign flipped, and the C operand's */
    def negated: FixDsp = copy(blocks = blocks.map(b => b.copy(negate = !b.negate)), negateC = c.isDefined && !negateC)

    private def f = blocks.head.b.hw.asInstanceOf[FixedPoint].fractional
    private def k = blocks.head.preShift
    /** Right shift from P to the result */
    private def s = f + k + shift
    private def n = blocks.size
    private def advA(i: Int) = (n - i) + 2 + (if blocks(i).d.isDefined then 1 else 0)
    private def advC = n + 1

    override def parents: Seq[(Sig[?], Int)] =
      blocks.zipWithIndex.flatMap((blk, i) =>
        Seq((blk.a, advA(i) + lead(blk.a))) ++ blk.d.map(d => (d, advA(i) + lead(d))) ++
          (blk.b match
            case _: Const[?] => None
            case b => Some((b, advA(i) + lead(b))))) ++
      c.map(c => (c, advC + lead(c)))

    override def implement(cp: (Sig[?], Int) => Component): Component =
      def in(sig: Sig[Double], adv: Int): Component = { val x = cp(sig, adv + lead(sig)); if fromDsp(sig) then x.keepRegister else x }
      val half = BigInt(1) << (s - 1)
      val last = blocks.zipWithIndex.foldLeft(Option.empty[ir.rtl.Dsp48]) { case (prev, (blk, i)) =>
        val first = prev.isEmpty
        Some(ir.rtl.Dsp48(
          a = in(blk.a, advA(i)), d = blk.d.map(in(_, advA(i))), subtractA = blk.subtractA,
          b = blk.b match
            case cst: Const[?] => Right(cst.bits)
            case b => Left(in(b, advA(i))),
          bWidth = blk.b.hw.size,
          c = if first then c.map(in(_, advC)) else None, cShift = f + k,
          pcin = prev, negateZ = first && negateC, negateM = blk.negate,
          rnd = if !first then BigInt(0) else if blk.negate then (BigInt(1) << 48) - half else half))
      }.get
      ir.rtl.Tap(last, s until (s + size))

  override def MID_VALUE: Double = valueOf(BigInt(1) << ((size - 1)/2))

  override def MAX_VALUE: Double = valueOf((BigInt(1) << (size - 1)) - 1)

  override def values: Iterator[Double] = BigIterator(0, BigInt(1)<<size).map(valueOf)

  override def toString: String = (magnitude, fractional) match
    case (8, 0) => "char"
    case (16, 0) => "short"
    case (32, 0) => "int"
    case (64, 0) => "long"
    case _ => s"FixedPoint($magnitude, $fractional)"
