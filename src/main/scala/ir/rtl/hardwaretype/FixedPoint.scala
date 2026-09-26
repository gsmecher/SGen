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
import ir.rtl.signals.{Const, Minus, Operator, Plus, Sig, Times}

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
  override def plus(lhs: Sig[Double], rhs: Sig[Double]): Sig[Double] = FixPlus(lhs, rhs)

  override def minus(lhs: Sig[Double], rhs: Sig[Double]): Sig[Double] = FixMinus(lhs, rhs)

  override def times(lhs: Sig[Double], rhs: Sig[Double]): Sig[Double] = (lhs, rhs) match
    // A sum or difference scaled by a negative power of two (as in a scaled butterfly): computed with the extra bits so that it never overflows.
    case (p: FixPlus, Const(value)) if rightShift(value, rhs.hw) > 0 => FixAddShift(p.lhs, p.rhs, subtract = false, rightShift(value, rhs.hw))
    case (p: FixMinus, Const(value)) if rightShift(value, rhs.hw) > 0 => FixAddShift(p.lhs, p.rhs, subtract = true, rightShift(value, rhs.hw))
    case _ => FixTimes(lhs, rhs)

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

  private case class FixPlus(override val lhs: Sig[Double], override val rhs: Sig[Double]) extends Plus(lhs, rhs):
    override def pipeline = 1

    override def implement(implicit cp: Sig[?] => Component) = ir.rtl.Plus(Seq(cp(this.lhs), cp(this.rhs)))

  private case class FixMinus(override val lhs: Sig[Double], override val rhs: Sig[Double]) extends Minus(lhs, rhs):
    override def pipeline = 1

    override def implement(implicit cp: Sig[?] => Component) = ir.rtl.Minus(cp(this.lhs), cp(this.rhs))

  private case class FixTimes(override val lhs: Sig[Double], override val rhs: Sig[Double]) extends Times(lhs, rhs):
    override def pipeline = this.rhs match
      case Const(value) if value > 0 && this.rhs.hw.bitsOf(value).bitCount == 1 => 0
      case _ => 3

    override def implement(implicit cp: Sig[?] => Component): Component =
      this.rhs match
        case Const(value) if value > 0 && this.rhs.hw.bitsOf(value).bitCount == 1 =>
          val shift = this.rhs.hw.bitsOf(value).lowestSetBit - this.rhs.hw.asInstanceOf[FixedPoint].fractional
          if shift > 0 then
            ir.rtl.Concat(Seq(ir.rtl.Tap(cp(this.lhs), 0 until (this.lhs.hw.size - shift)),ir.rtl.Const(shift,0)))
          else
            val input = cp(this.lhs)
            val rightShift = -shift
            val sign = ir.rtl.Tap(input, (this.lhs.hw.size - 1) until this.lhs.hw.size)
            ir.rtl.Concat(
              Seq.fill(rightShift)(sign) :+
                ir.rtl.Tap(input, rightShift until this.lhs.hw.size)
            )
        case _ =>
          val shift = this.rhs.hw.asInstanceOf[FixedPoint].fractional
          ir.rtl.Tap(ir.rtl.Times(cp(this.lhs), cp(this.rhs)), shift until (shift + this.lhs.hw.size))

  /**
   * Sum or difference of two signals, shifted right by `shift` bits (a scaled butterfly). The sum is computed on `shift` more bits
   * than the operands, so that it cannot overflow, and rounded if FixedPoint.rounding.
   *
   * For a shift of one bit, the rounding is convergent (ties to even): a sum that is halved is a tie half of the time, so that rounding
   * ties upwards would bias the result by a quarter of a bit per stage. Whether the result must be incremented only depends on the two
   * low bits of the operands; it is computed one cycle ahead (from the operands one cycle before they enter the adder) and registered,
   * so that it enters the adder as a carry input coming straight from a register (for a difference, as a third operand). For larger
   * shifts, ties are rare and half of the last kept bit is added before the shift.
   */
  private case class FixAddShift(lhs: Sig[Double], rhs: Sig[Double], subtract: Boolean, shift: Int) extends Sig[Double](using lhs.hw):
    override val hash: Int = Seq("FixAddShift", lhs, rhs, subtract, shift).hashCode()

    override def pipeline = 1

    private def convergent = FixedPoint.rounding && shift == 1

    override def parents: Seq[(Sig[?], Int)] = if convergent then Seq((lhs, 0), (rhs, 0), (lhs, 1), (rhs, 1)) else Seq((lhs, 0), (rhs, 0))

    override def implement(cp: (Sig[?], Int) => Component): Component =
      val (a, b) = (cp(lhs, 0), cp(rhs, 0))
      def extend(c: Component) = ir.rtl.Concat(Seq.fill(shift)(ir.rtl.Tap(c, (size - 1) until size)) :+ c)
      def bit(c: Component, i: Int) = ir.rtl.Tap(c, i until (i + 1))
      val rounded =
        if !FixedPoint.rounding then
          if subtract then ir.rtl.Minus(extend(a), extend(b)) else ir.rtl.Plus(Seq(extend(a), extend(b)))
        else if convergent then
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

  override def MID_VALUE: Double = valueOf(BigInt(1) << ((size - 1)/2))

  override def MAX_VALUE: Double = valueOf((BigInt(1) << (size - 1)) - 1)

  override def values: Iterator[Double] = BigIterator(0, BigInt(1)<<size).map(valueOf)

  override def toString: String = (magnitude, fractional) match
    case (8, 0) => "char"
    case (16, 0) => "short"
    case (32, 0) => "int"
    case (64, 0) => "long"
    case _ => s"FixedPoint($magnitude, $fractional)"

object FixedPoint:
  /** Whether the results of scaled butterflies are rounded to nearest rather than truncated */
  var rounding: Boolean = true
