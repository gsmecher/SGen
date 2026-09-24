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

import ir.rtl.{Component, Concat, Const as RTLConst, Dsp48, Input as RTLInput, Minus as RTLMinus, Plus as RTLPlus, Register, Tap}
import ir.rtl.signals.{Const, Input, Sig}
import org.scalatest.funsuite.AnyFunSuite

class FixedPointTest extends AnyFunSuite:
  test("bitsOf should work with a set of doubles"):
    val hw = FixedPoint(2, 1)
    assertThrows[IllegalArgumentException](hw.bitsOf(Double.NegativeInfinity))
    assertThrows[IllegalArgumentException](hw.bitsOf(Double.MinValue))
    assertThrows[IllegalArgumentException](hw.bitsOf(-2.5d))
    assert(hw.bitsOf(-2d) == 4)
    assert(hw.bitsOf(-1.5d) == 5)
    assert(hw.bitsOf(-1d) == 6)
    assert(hw.bitsOf(-0.5d) == 7)
    assert(hw.bitsOf(-Double.MinPositiveValue) == 0)
    assert(hw.bitsOf(-0d) == 0)
    assert(hw.bitsOf(0d) == 0)
    assert(hw.bitsOf(Double.MinPositiveValue) == 0)
    assert(hw.bitsOf(0.5d) == 1)
    assert(hw.bitsOf(1d) == 2)
    assert(hw.bitsOf(1.5d) == 3)
    assertThrows[IllegalArgumentException](hw.bitsOf(2d))
    assertThrows[IllegalArgumentException](hw.bitsOf(Double.MaxValue))
    assertThrows[IllegalArgumentException](hw.bitsOf(Double.PositiveInfinity))
    assertThrows[IllegalArgumentException](hw.bitsOf(Double.NaN))
    
  test("valueOf should work with a set of inputs"):
    val hw = FixedPoint(2, 1)
    assert(hw.valueOf(0) == 0d)
    assert(hw.valueOf(1) == 0.5d)
    assert(hw.valueOf(2) == 1d)
    assert(hw.valueOf(3) == 1.5d)
    assert(hw.valueOf(4) == -2d)
    assert(hw.valueOf(5) == -1.5d)
    assert(hw.valueOf(6) == -1d)
    assert(hw.valueOf(7) == -0.5d)
    assertThrows[IllegalArgumentException](hw.valueOf(8))

  test("fractional power-of-two products should arithmetic-shift negatives"):
    given hw: FixedPoint = FixedPoint(27, 3)
    val input = Input[Double](0)
    val inputComponent = RTLInput(hw.size, "input")

    def lower(product: Sig[Double]): Component =
      product.implement((signal, _) =>
        if signal == input then inputComponent
        else throw IllegalArgumentException(s"Unexpected signal $signal")
      )

    def evaluate(component: Component, inputBits: BigInt): BigInt =
      component match
        case current if current == inputComponent => inputBits
        case RTLConst(_, value) => value
        case Tap(parent, range) =>
          (evaluate(parent, inputBits) >> range.start) &
            ((BigInt(1) << range.size) - 1)
        case Concat(parts) =>
          parts.foldLeft(BigInt(0))((result, part) =>
            (result << part.size) | evaluate(part, inputBits)
          )
        case other => throw IllegalArgumentException(s"Unexpected component $other")

    val mask = (BigInt(1) << hw.size) - 1
    def bits(raw: BigInt): BigInt = raw & mask

    val half = lower(input * Const(0.5))
    assert(evaluate(half, bits(-12)) == bits(-6))
    assert(evaluate(half, bits(12)) == bits(6))

    val quarter = lower(input * Const(0.25))
    assert(evaluate(quarter, bits(-10)) == bits(-3))
    assert(evaluate(quarter, bits(10)) == bits(2))

  /** Components of the implementation, from the output */
  private def components(sig: Sig[Double], inputs: Map[Sig[?], Component]): Seq[Component] =
    def all(comp: Component): Seq[Component] = comp +: comp.parents.flatMap(all)
    all(sig.implement((signal, _) => inputs(signal))).distinct

  private def blocks(comps: Seq[Component]): Seq[Dsp48] = comps.collect { case d: Dsp48 => d }

  test("a product should be one instantiated DSP48E2 block with its registers, rounding on W"):
    given hw: FixedPoint = FixedPoint(2, 16)
    val (a, b) = (Input[Double](0), Input[Double](1))
    val twiddle = FixedPoint(1, 17, saturating = true)
    val c = Const(0.7071)(using twiddle)
    val inputs = Map[Sig[?], Component](a -> RTLInput(hw.size, "a"), b -> RTLInput(hw.size, "b"))

    val byConst = a * c
    assert(byConst.getClass.getSimpleName == "FixDsp")
    assert(byConst.parents == Seq((a, 3))) // AREG, MREG, PREG
    val cb = blocks(components(byConst, inputs))
    assert(cb.size == 1 && cb.head.d.isEmpty && cb.head.b == Right(twiddle.bitsOf(0.7071)) && cb.head.bWidth == twiddle.size && cb.head.c.isEmpty && cb.head.pcin.isEmpty)
    assert(cb.head.breg == 0 && !cb.head.negateM && !cb.head.negateZ && cb.head.rnd == (BigInt(1) << 16)) // half of the 17 fractional bits dropped
    assert(components(byConst, inputs).count(_.isInstanceOf[Register]) == 0) // every register is inside the block

    val w = Input[Double](2)(using twiddle)
    val byVar = a * w
    assert(byVar.parents == Seq((a, 3), (w, 3)))
    val vb = blocks(components(byVar, inputs + (w -> RTLInput(twiddle.size, "w"))))
    assert(vb.size == 1 && vb.head.b.isLeft && vb.head.breg == 1)

    // A power of two is a wire
    assert((a * Const(0.5)(using twiddle)).getClass.getSimpleName == "FixTimes")
    assert((a * Const(0.25)).getClass.getSimpleName == "FixTimes")

  test("sums and differences of two products by the same constant should be products of the sum (DSP pre-adder)"):
    given hw: FixedPoint = FixedPoint(2, 16)
    val (a, b) = (Input[Double](0), Input[Double](1))
    val twiddle = FixedPoint(1, 17, saturating = true)
    val c = Const(0.7071)(using twiddle)
    val inputs = Map[Sig[?], Component](a -> RTLInput(hw.size, "a"), b -> RTLInput(hw.size, "b"))

    for subtract <- Seq(false, true) do
      val fused = if subtract then a * c - b * c else a * c + b * c
      assert(fused.getClass.getSimpleName == "FixDsp")
      assert(fused.parents.toSet == Set((a, 4), (b, 4))) // ADREG in front of MREG, PREG; AREG/DREG
      val bl = blocks(components(fused, inputs))
      assert(bl.size == 1 && bl.head.preAdd && bl.head.subtractA == subtract && bl.head.b == Right(twiddle.bitsOf(0.7071)))
      // (a - b) c: D = a, A = b (the pre-adder forms D - A)
      assert(Set(bl.head.d.get, bl.head.a) == Set(inputs(a), inputs(b)))
      if subtract then assert(bl.head.d.get == inputs(a) && bl.head.a == inputs(b))

    // Different constants, a variable factor: two blocks on one cascade; a power of two (a wire) keeps a fabric adder
    val two = a * c + b * Const(0.6)(using twiddle)
    assert(two.getClass.getSimpleName == "FixDsp")
    assert(two.parents == Seq((a, 4), (b, 3))) // the first block's operands one cycle ahead of the second's
    val tb = blocks(components(two, inputs)) // from the output: the second block first
    assert(tb.size == 2 && tb.head.pcin.contains(tb.last) && tb.last.rnd == (BigInt(1) << 16) && tb.head.rnd == 0)
    val diff = blocks(components(a * c - b * Const(0.6)(using twiddle), inputs))
    assert(diff.head.negateM && !diff.last.negateM)
    // A power of two (a wire) is an addend: it rides on the block's C input
    val wireAddend = a * c + b * Const(0.5)(using twiddle)
    assert(wireAddend.getClass.getSimpleName == "FixDsp" && wireAddend.parents.map(_._2) == Seq(3, 2)) // a at AREG+MREG+PREG, the wire at CREG+PREG
    // Constants wider than the 18-bit port keep two multipliers
    val wide = Const(0.7071)(using FixedPoint(2, 22))
    assert(blocks(components(a * wide + b * wide, inputs)).size == 2)

  test("a butterfly on a pre-added product should stay in the product's DSP block (output adder, C input)"):
    given hw: FixedPoint = FixedPoint(2, 16)
    val (a, b, d) = (Input[Double](0), Input[Double](1), Input[Double](2))
    val twiddle = FixedPoint(1, 17, saturating = true)
    val c = Const(0.7071)(using twiddle)
    val p = a * c + b * c
    val inputs = Map[Sig[?], Component](a -> RTLInput(hw.size, "a"), b -> RTLInput(hw.size, "b"), d -> RTLInput(hw.size, "d"))
    for (fused, negateZ, negateM) <- Seq((d + p, false, false), (p + d, false, false), (d - p, false, true), (p - d, true, false)) do
      assert(fused.getClass.getSimpleName == "FixDsp")
      assert(fused.parents == Seq((a, 4), (b, 4), (d, 2))) // CREG, PREG
      val bl = blocks(components(fused, inputs))
      assert(bl.size == 1 && bl.head.c.contains(inputs(d)) && bl.head.cShift == 17)
      assert(bl.head.negateZ == negateZ && bl.head.negateM == negateM)
      // The rounding constant: half of the last kept bit, negated with the product so that the rounding adds half a unit either way
      assert(bl.head.rnd == (if negateM then (BigInt(1) << 48) - (BigInt(1) << 16) else BigInt(1) << 16))
    // A scaled butterfly folds into the block's output slice, the rounding constant following the slice
    val halved = (d + p) * Const(0.5)
    assert(halved.getClass.getSimpleName == "FixDsp")
    assert(blocks(components(halved, inputs)).head.rnd == (BigInt(1) << 17))
    assert(blocks(components((d + p) * Const(0.25), inputs)).head.rnd == (BigInt(1) << 18))
    assert(blocks(components(halved * Const(0.5), inputs)).head.rnd == (BigInt(1) << 18)) // scaled twice: still one slice
    // A block fed by another block's output gets a fabric register in front (lead), through a power-of-two wire too
    assert((halved * c).parents == Seq((halved, 4)))
    assert((halved * Const(2.0)(using FixedPoint(3, 15)) * c).parents.map(_._2) == Seq(4))
    // The opposite of a chain flips its signs and keeps the C input free
    val opp = Const(0.0) - p
    assert(opp.getClass.getSimpleName == "FixDsp" && blocks(components(opp, inputs)).head.negateM && (d + opp).getClass.getSimpleName == "FixDsp")
    // A block with its C input taken keeps a fabric adder for a further operand
    assert((d + p + b).getClass.getSimpleName == "FixPlus")
