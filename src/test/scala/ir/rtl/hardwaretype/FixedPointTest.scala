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

import ir.rtl.{Component, Concat, Const as RTLConst, Input as RTLInput, Tap}
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
