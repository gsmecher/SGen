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

import ir.rtl.hardwaretype.{ComplexHW, FixedPoint, Flopoco, HW, IEEE754}
import ir.rtl.{AcyclicStreamingModule, StreamingModule}
import ir.spl.*
import maths.fields.{Complex, F2}
import maths.linalg.Matrix
import transforms.{HighLevelTransform, Transform, perm}
import transforms.perm.LinearPerm
import transforms.perm.LinearPerm.{*, given}

import scala.language.implicitConversions
import scala.math.Numeric.Implicits.infixNumericOps


object DFT:
  def omega(n: Int, pow: Int): Complex[Double] =
   if pow % (1 << n) == 0 then
     Complex(1)
   else if 2 * (pow % (1 << n)) == (1 << n) then
     Complex(-1)
   else if 4 * (pow % (1 << n)) == (1 << n) then
     Complex(0,-1)
   else if 4 * (pow % (1 << n)) == 3 * (1 << n) then
     Complex(0,1)
   else
     val angle = -2 * Math.PI * pow / (1 << n)
     Complex(Math.cos(angle), Math.sin(angle))

  /**
   * Radices of a mixed-radix Cooley-Tukey FFT that uses the radix 2^rMax as often as possible. The remainder, if any,
   * forms the last stage: the first stage requires no twiddle factors whatever its radix, so the stage without twiddles
   * should be one of the large radix (the smaller permutations following a small last stage cost much less).
   *
   * @param n    Log of the size of the transform
   * @param rMax Log of the preferred radix
   * @return Log of the radix of each stage, in the order the stages are applied to the data
   */
  def greedyRadices(n: Int, rMax: Int): Seq[Int] =
    require(n > 0 && rMax > 0, s"n ($n) and rMax ($rMax) must be strictly positive")
    val r = Math.min(n, rMax)
    Seq.fill(n / r)(r) ++ (if n % r == 0 then Seq() else Seq(n % r))

abstract class DFT(n: Int) extends HighLevelTransform[Complex[Double]](n):
  override def testParams: PartialFunction[HW[Complex[Double]], (Seq[Complex[Double]], Double)] =
    case ComplexHW(hw@FixedPoint(magnitude, fractional)) if hw.MID_VALUE*(1<<n) <= hw.MAX_VALUE => (testInputs(hw), (BigInt(1)<<(n + 1)).toDouble / (BigInt(1) << fractional).toDouble)
    case ComplexHW(hw@Flopoco(wE, wF)) if wE <= 11 && wF <= 52 && hw.MID_VALUE * (1 << n) <= hw.MAX_VALUE => (testInputs(hw), (BigInt(1) << (n + 1)).toDouble / (BigInt(1) << wF).toDouble)
    case ComplexHW(hw@IEEE754(wE, wF)) if wE <= 11 && wF <= 52 && hw.MID_VALUE * (1 << n) <= hw.MAX_VALUE => (testInputs(hw), (BigInt(1) << (n + 1)).toDouble / (BigInt(1) << wF).toDouble)

  def testInputs(hw: HW[Double]):Seq[Complex[Double]] =
    val inputs0 = Seq.fill(1 << n)(Complex(0.0)) // First test input set contains only ones
    val inputs1 = Seq.fill(1 << n)(Complex(hw.MID_VALUE)) // Second set contains only ones
    val inputs2 = Seq.tabulate(1 << n)(k => DFT.omega(n, (1 << n) - k) * hw.MID_VALUE) // Third set is e^(2ikPI/2^n)
    val inputs3 = Seq.tabulate(1 << n)(k => if k == 0 then Complex(hw.MID_VALUE) else Complex(0.0)) // Fourth set is a dirac
    inputs0 ++ inputs1 ++ inputs2 ++ inputs3
  
/**
 * Mixed-radix Cooley-Tukey FFT.
 *
 * @param n             Log of the size of the transform
 * @param rs            Log of the radix of each stage, in the order the stages are applied to the data (the first stage
 *                      requires no twiddle factors). The sum must be n.
 * @param scalingFactor Scaling factor applied by each radix-2 butterfly
 */
case class CTDFT(override val n: Int, rs: Seq[Int], scalingFactor: Complex[Double]) extends DFT(n):
  require(rs.nonEmpty && rs.forall(_ > 0) && rs.sum == n, s"radices ($rs) must be strictly positive and sum up to n ($n)")

  override val spl: SPL[Complex[Double]] =
    if n == 1 then
      DFT2(scalingFactor)
    else
      val stages = rs.reverse // SPL factors, and the stage index l of DiagE, Qmat and Rmat, are in product order: the stage applied last comes first.
      Lmat(stages.head, n) * Product(stages.size)(l => ITensor(n - stages(l), CTDFT(stages(l), 1, scalingFactor).spl) * DiagE(n, stages, l) * Qmat(n, stages, l)) * Rmat(n, stages)

object CTDFT:
  /** Cooley-Tukey FFT using the radix 2^r as often as possible (uniform radix-2^r FFT if r divides n). */
  def apply(n: Int, r: Int, scalingFactor: Complex[Double]): CTDFT = CTDFT(n, DFT.greedyRadices(n, r), scalingFactor)

/** Mixed-radix inverse Cooley-Tukey FFT, see [[CTDFT]]. */
case class ICTDFT(override val n: Int, rs: Seq[Int], scalingFactor: Complex[Double]) extends DFT(n):
  override val spl: SPL[Complex[Double]] = Swap(n) * CTDFT(n, rs, scalingFactor).spl * Swap(n)

object ICTDFT:
  /** Inverse Cooley-Tukey FFT using the radix 2^r as often as possible (uniform radix-2^r FFT if r divides n). */
  def apply(n: Int, r: Int, scalingFactor: Complex[Double]): ICTDFT = ICTDFT(n, DFT.greedyRadices(n, r), scalingFactor)

case class Pease(override val n: Int, r: Int, scalingFactor: Complex[Double]) extends DFT(n):
  require(n % r == 0, s"n ($n) must be a multiple of r ($r)")
  override val spl: SPL[Complex[Double]] =
    if n == 1 then
      DFT2(scalingFactor)
    else
      Rmat(r, n) * Product(n / r)(l => DiagC(n, r, n / r - l - 1) * ITensor(n - r, CTDFT(r, 1, scalingFactor).spl) * Lmat(r, n).inverse)
    
case class ItPease(override val n: Int, r: Int, scalingFactor: Complex[Double]) extends DFT(n):
  require(n % r == 0, s"n ($n) must be a multiple of r ($r)")
  override val spl =
    if n == 1 then
      DFT2(scalingFactor)
    else
      Rmat(r, n) * ItProduct(n / r, StreamDiagC(n, r) * ITensor(n - r, CTDFT(r, 1, scalingFactor).spl) * Lmat(r, n).inverse)

case class ItPeaseFused(override val n: Int, r: Int, scalingFactor: Complex[Double]) extends DFT(n):
  require(n % r == 0, s"n ($n) must be a multiple of r ($r)")
  override val spl =
    if n == 1 then
      DFT2(scalingFactor)
    else
      ItProduct(n / r + 1, perm.LinearPerm(Seq.fill(n / r)(Lmat(r, n).inverse) :+ Rmat(r, n)), Some(StreamDiagC(n, r) * ITensor(n - r, CTDFT(r, 1, scalingFactor).spl)))
  //def stream(n: Int, r: Int, k: Int, hw: HW[Complex[Double]],dualPorted:Boolean): StreamingModule[Complex[Double]] = CTDFT(n, r).stream(k)(hw)

case class IItPeaseFused(override val n: Int, r: Int, scalingFactor: Complex[Double]) extends DFT(n):
  override val spl = Swap(n) * ItPeaseFused(n, r, scalingFactor) * Swap(n)

/** Dummy module used for representation in graphs 
case class DFT(_t:Int, _k:Int) extends AcyclicStreamingModule(_t, _k)(using ComplexHW(FixedPoint(8,8))):
  override def implement(inputs: Seq[ir.rtl.signals.Sig[Complex[Double]]]) = ???

  override def spl = DFT.CTDFT(t + k, 1, 1)
*/