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

import ir.rtl.hardwaretype.{ComplexHW, FixedPoint, HW}
import ir.rtl.{AcyclicStreamingModule, StreamingModule,RAMControl}
import ir.rtl.signals.{ROM, Sig, Timer}
import ir.spl.{Identity, Repeatable, SPL}
import maths.fields.{Complex, F2}
import maths.fields.Complex._
import maths.linalg.Matrix
import transforms.perm.LinearPerm

/**
 * Twiddle factors for non-iterative Cooley-Tukey FFTs
 *
 * The stage combines 2^r sub-transforms of size 2^(n - s - r) into transforms of size 2^(n - s), and therefore multiplies
 * by powers of the 2^(n - s)-th root of unity. The s most significant bits index the groups and are left untouched.
 *
 * @param n Log of the size of the transform
 * @param r Log of the radix of the stage
 * @param s Number of bits already processed by the stages applied after this one (r times the stage number for a uniform radix)
 * @param before The permutation this diagonal has been moved in front of, if any: the diagonal is defined on the
 *               positions after that permutation, so the element at position j here gets the coefficient of position
 *               P j (see CTDFT: a diagonal commutes with a permutation up to this re-indexing of its entries).
 */
case class DiagE private (override val n: Int, r: Int, s: Int, before: Option[Matrix[F2]]) extends SPL[Complex[Double]](n) with Repeatable[Complex[Double]]:
  val num = Numeric[Complex[Double]]
  import num._
  def pow(x: Int): Int =
    val j = x % (1 << r)
    val i = (x >> r) % (1 << (n - s - r))
    (i * j) << s

  def coef(i: Int): Complex[Double] = DFT.omega(n, pow(before match
    case Some(p) => LinearPerm.permute(p, i)
    case None => i))

  override def eval(inputs: Seq[Complex[Double]], set: Int): Seq[Complex[Double]] = inputs.zipWithIndex.map((input, i) => input * coef(i % (1 << n)))

  override def stream(k: Int,control:RAMControl)(using HW[Complex[Double]]): AcyclicStreamingModule[Complex[Double]] = new AcyclicStreamingModule(n - k, k): 
    override def implement(inputs: Seq[Sig[Complex[Double]]]): Seq[Sig[Complex[Double]]] = (0 until K).map(p => 
      val twiddles = Vector.tabulate(T)(c => coef((c * K) + p))
      val twiddleHW = hw match // The hardware datatype used for the twiddles is the same as the one used by the data, EXCEPT in case of FixedPoint: to maximize precision, we store as many fractional bits as possible, as twiddles are in the unit circle. 
        case ComplexHW(FixedPoint(magnitude, fractional, _)) =>
          val f = DiagE.twiddleFractional.getOrElse(magnitude + fractional - 2)
          if DiagE.twiddleSaturate then ComplexHW(FixedPoint(1, f, saturating = true)) else ComplexHW(FixedPoint(2, f))
        case _ => hw
      val control = Timer(T)
      val twiddle = ROM(twiddles, control)(using twiddleHW)
      inputs(p) * twiddle)

    override def toString: String = "DiagE(" + this.n + "," + r + "," + s + "," + this.k + ")"

    override def spl: SPL[Complex[Double]] = new DiagE(this.n, r, s, before)

/** Companion object of class DiagE */
object DiagE:
  /** Fractional bits of fixed-point twiddles, when they should differ from the data's (-twiddle). With 2^k-point
   *  data words the twiddles otherwise get the same width; a DSP48E2 multiplies 27 x 18 bits, so 18-bit data can
   *  meet twiddles of up to 27 bits at no multiplier cost (the twiddle ROMs grow). */
  var twiddleFractional: Option[Int] = None
  /** Store twiddles with one integer bit (the sign), 1.0 saturated to 1 - 2^-f (-twiddlesat): one more fractional bit in
   *  the same word, at a relative error of 2^-f on the exact-1.0 entries that are not wired trivially. */
  var twiddleSaturate: Boolean = false


  /**
   * Twiddle factors of stage l of a uniform radix-2^r Cooley-Tukey FFT.
   *
   * @param n Log of the size of the transform
   * @param r Log of the radix
   * @param l Stage number
   */
  def apply(n: Int, r: Int, l: Int): SPL[Complex[Double]] = apply(n, Seq.fill(n / r)(r), l)

  /**
   * Twiddle factors of stage l of a mixed-radix Cooley-Tukey FFT.
   *
   * @param n      Log of the size of the transform
   * @param rs     Log of the radix of each stage, stage 0 being the leftmost factor (i.e. the last one applied)
   * @param l      Stage number
   * @param before The permutation this diagonal has been moved in front of, if any (see the class documentation)
   */
  def apply(n: Int, rs: Seq[Int], l: Int, before: Option[Matrix[F2]] = None): SPL[Complex[Double]] =
    val s = rs.take(l).sum
    if n == s + rs(l) then
      Identity[Complex[Double]](n)
    else
      new DiagE(n, rs(l), s, before)
