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

package transforms.fft

import helpers.Xsim.test
import helpers.Vivado.synthetize
import ir.rtl.hardwaretype.{ComplexHW, FixedPoint, Flopoco, HW, IEEE754}
import maths.fields.Complex
import Complex.*
import ir.rtl.RAMControl
import ir.rtl.RAMControl.{Single, SinglePorted}
import ir.spl.SPL
import org.scalatest.*
import org.scalatest.funsuite.AnyFunSuite
import scala.collection.parallel.CollectionConverters.*
import scala.math.Fractional.Implicits.*


class DFTTest extends AnyFunSuite:
  val standardHWs = Vector(
    FixedPoint(8, 0),
    FixedPoint(16, 0),
    FixedPoint(32, 0),
    FixedPoint(64, 0),
    Flopoco(5, 10),
    Flopoco(8, 7),
    Flopoco(8, 23),
    Flopoco(11, 52),
    IEEE754(5, 10),
    IEEE754(8, 7),
    IEEE754(8, 23),
    IEEE754(11, 52)
  )

  val designs = Vector(
    ("CT", (n: Int, r: Int) => CTDFT(n, r, 1), false, false),
    ("ICT", (n: Int, r: Int) => ICTDFT(n, r, 1), false, true),
    ("Pease", Pease(_, _, 1), false, false),
    ("ItPease", ItPease(_, _, 1), true, false),
    ("ItPeaseFused", ItPeaseFused(_, _, 1), true, false),
    ("IItPeaseFused", IItPeaseFused(_, _, 1), true, true)
  )

  for
    (name, uut, _, inverse) <- designs
    n <- 1 to 10
    r <- 1 until n if n % r == 0
  do
    test(s"Checking $name FFT (size ${1 << n} radix ${1 << r})"):
      val sb = uut(n, r)
      for
        j <- (0 until 1 << n).par
        res = sb.eval(Seq.tabulate(1 << n)(i => if (i == j) 1.0 else 0.0), 0).toVector
        i <- 0 until 1 << n
      do
        val v = res(i) - (if inverse then DFT.omega(n, -i * j) else DFT.omega(n, i * j))
        assert(v.norm2 < 0.00001)

  for
    (name, uut) <- Vector(("ICT", (n: Int, r: Int, sf: Complex[Double]) => ICTDFT(n, r, sf)),("IItPeaseFused", (n: Int, r: Int, sf: Complex[Double]) => IItPeaseFused(n, r, sf)))
    n <- 1 to 10
    r <- 1 until n if n % r == 0
  do
    test(s"Checking that $name (size ${1 << n} radix ${1 << r}) is correctly scaled"):
      val sb = uut(n, r, 0.5)
      for
        j <- (0 until 1 << n).par
        res = sb.eval(Seq.tabulate(1 << n)(i => DFT.omega(n, i * j)), 0).toVector
        i <- 0 until 1 << n
      do
        val expected = if (i == j) 1.0 else 0.0
        val v = res(i) - expected
        assert(v.norm2 < 0.00001, s"${res(i)} is different than expected ($expected).")

  for
    (name, uut, iterative, _) <- designs
    t <- 1 to 3
    k <- 1 to 3
    n = t + k
    r <- 1 until n if n % r == 0 && (!iterative || k >= r)
    dft = uut(n, r)
    inner <- standardHWs
    hw = ComplexHW(inner) if dft.testParams.isDefinedAt(hw)
    dp <- RAMControl.values if dp == RAMControl.Dual || !iterative
  do
    val description = s"$name FFT (size 2^$n, 2^$k ports, radix 2^$r, $dp RAM banks, $hw)"
    test(s"Generating $description", Tag("simulation")):
      val (testInputs, epsilon) = dft.testParams(hw)
      dft.test(k, dp, hw, s"$name-$n-$r-$k-$dp-$inner")

  for
    (name, uut, iterative, _) <- designs
    t <- 1 to 5
    k <- 1 to 3
    n = t + k if n < 6
    r <- 1 until n if n % r == 0 && (!iterative || k >= r)
    inner <- standardHWs
    hw = ComplexHW(inner)
    dp <- RAMControl.values if dp == RAMControl.Dual || !iterative
  do
    val description = s"$name FFT (size 2^$n, 2^$k ports, radix 2^$r, $dp RAM banks, $hw)"
    test(s"Generating testbench for $description", Tag("synthesis")):
      uut(n, r).stream(k, dp)(using hw).synthetize(s"$name-$n-$r-$k-$dp-$inner")

  // Mixed-radix Cooley-Tukey FFTs. Radices are listed in the order the stages are applied to the data.
  test("Greedy radix decomposition"):
    assert(DFT.greedyRadices(9, 3) == Seq(3, 3, 3))
    assert(DFT.greedyRadices(11, 3) == Seq(3, 3, 3, 2))
    assert(DFT.greedyRadices(10, 4) == Seq(4, 4, 2))
    assert(DFT.greedyRadices(5, 8) == Seq(5))
    assert(DFT.greedyRadices(7, 1) == Seq.fill(7)(1))
    assert(CTDFT(11, 3, 1) == CTDFT(11, Seq(3, 3, 3, 2), 1))

  val mixedRadices = Vector(Seq(3, 2), Seq(2, 3), Seq(1, 3), Seq(3, 1), Seq(3, 3, 1), Seq(1, 3, 3), Seq(3, 1, 3), Seq(2, 1, 3), Seq(1, 2, 3), Seq(3, 2, 1), Seq(3, 3, 2), Seq(2, 2, 3, 1), Seq(4, 3, 2, 1), Seq(3, 3, 3, 1))

  for
    (name, uut, inverse) <- Vector(("CT", (n: Int, rs: Seq[Int]) => CTDFT(n, rs, 1), false), ("ICT", (n: Int, rs: Seq[Int]) => ICTDFT(n, rs, 1), true))
    rs <- mixedRadices
    n = rs.sum
  do
    test(s"Checking mixed-radix $name FFT (size ${1 << n} radices ${rs.map(1 << _).mkString(",")})"):
      val sb = uut(n, rs)
      for
        j <- (0 until 1 << n).par
        res = sb.eval(Seq.tabulate(1 << n)(i => if (i == j) 1.0 else 0.0), 0).toVector
        i <- 0 until 1 << n
      do
        val v = res(i) - (if inverse then DFT.omega(n, -i * j) else DFT.omega(n, i * j))
        assert(v.norm2 < 0.00001)

  for
    rs <- mixedRadices if rs.sum <= 7
    n = rs.sum
    k <- 1 to 3
    dft = CTDFT(n, rs, 1)
    inner <- Vector(FixedPoint(16, 0), Flopoco(8, 23), IEEE754(8, 23))
    hw = ComplexHW(inner) if dft.testParams.isDefinedAt(hw)
    dp <- RAMControl.values
  do
    val description = s"mixed-radix CT FFT (size 2^$n, 2^$k ports, radices ${rs.map(1 << _).mkString(",")}, $dp RAM banks, $hw)"
    test(s"Generating $description", Tag("simulation")):
      dft.test(k, dp, hw, s"MixedCT-$n-${rs.mkString("")}-$k-$dp-$inner")

  for
    rs <- Vector(Seq(3, 2), Seq(2, 3), Seq(3, 1, 1), Seq(1, 1, 3))
    n = rs.sum
    k <- 1 to 3
    inner <- Vector(FixedPoint(16, 0))
    hw = ComplexHW(inner)
    dp <- RAMControl.values
  do
    val description = s"mixed-radix CT FFT (size 2^$n, 2^$k ports, radices ${rs.map(1 << _).mkString(",")}, $dp RAM banks, $hw)"
    test(s"Generating testbench for $description", Tag("synthesis")):
      CTDFT(n, rs, 1).stream(k, dp)(using hw).synthetize(s"MixedCT-$n-${rs.mkString("")}-$k-$dp-$inner")
