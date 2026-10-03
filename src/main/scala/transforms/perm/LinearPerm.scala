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

package transforms.perm

import ir.rtl.hardwaretype.HW
import ir.rtl.{RAMControl, StreamingModule}
import ir.spl.SPL
import maths.fields.F2
import maths.linalg.{LUL, Matrix, Vec}
import transforms.Transform
import transforms.perm.{Spatial, Temporal}

case class LinearPerm[T](P: Seq[Matrix[F2]]) extends Transform[T](P.head.m):
  private val ULU = false
  assert(P.forall(m => m.m == m.n))
  assert(P.forall(_.isInvertible))
  assert(P.forall(m => m.m == n))

  override def eval(inputs: Seq[T], set: Int): Seq[T] = LinearPerm.permute(P(set % P.size), inputs) //inputs.grouped(N).toSeq.zipWithIndex.flatMap { case (inputs, s) => LinearPerm.permute(P(s % P.size), inputs) }

  override def stream(k: Int, control:RAMControl)(implicit hw: HW[T]): StreamingModule[T] = 
    def unblock(P: Matrix[F2], t: Int) = 
      assert(P.m == P.n)
      val k = P.m - t
      val P4 = P(0 until t, 0 until t)
      val P3 = P(0 until t, t until k + t)
      val P2 = P(t until k + t, 0 until t)
      val P1 = P(t until k + t, t until k + t)
      (P1, P2, P3, P4)

    val t = n - k

    val ps = P.map(p => unblock(p, t))
    val p1 = ps.map(_._1)
    val p2 = ps.map(_._2)
    val p3 = ps.map(_._3)
    val p4 = ps.map(_._4)
    if !ULU then 
      val L2 = P.map(p => new LUL(p, t, k).getSolution)
      val L1 = Vector.tabulate(P.size)(i => p1(i) + L2(i) * p3(i))
      val C4 = Vector.tabulate(P.size)(i => p4(i) + p3(i) * (p1(i) + L2(i) * p3(i)).inverse * (p2(i) + L2(i) * p4(i)))
      val C3 = p3
      val R2 = Vector.tabulate(P.size)(i => (p1(i) + L2(i) * p3(i)).inverse * (p2(i) + L2(i) * p4(i)))


      Spatial(L1, L2) *
        Temporal(C3, C4,control) *
        Spatial(Vector.fill(P.size)(Matrix.identity[F2](k)), R2)
    else 
      val L = new LUL((p1.head :: p2.head) / (p3.head :: p4.head), k, t).getSolution
      val R3 = p3.head + L * p1.head
      val R4 = p4.head + L * p2.head
      val C2 = p2.head * R4.inverse
      val C1 = p1.head + C2 * R3
      Temporal(L, Matrix.identity[F2](t),control) *
        Spatial(C1, C2) *
        Temporal(R3, R4,control)

  override def testParams: PartialFunction[HW[T], (Seq[T], Double)] =
    case hw =>
      def rep: Iterator[T] = hw.values ++ rep
      (rep.take((1 << n) * P.size * 5).toSeq, 0)


object LinearPerm:
  def apply[T](P: Matrix[F2]) = new LinearPerm[T](Seq(P))

  given [T]: Conversion[Matrix[F2], Transform[T]] with
    def apply(P: Matrix[F2]): Transform[T] = LinearPerm(Seq(P))

  def permute[T](P: Matrix[F2], v: Seq[T]): Seq[T] = 
    val Pinv = P.inverse
    Vector.tabulate(1 << P.m)(i => v(permute(Pinv, i)))

  def permute(P: Matrix[F2], i: Int): Int = (P * Vec.fromInt(P.m, i)).toInt

  /** Digit reversal of n bits grouped in digits of r bits (bit reversal if r == 1). r must divide n. */
  def Rmat(r: Int, n: Int): Matrix[F2] =
    require(n % r == 0, s"n ($n) must be a multiple of r ($r)")
    Rmat(n, Seq.fill(n / r)(r))

  /**
   * Mixed-radix digit reversal, as used at the input of a Cooley-Tukey FFT with radices 2^rs(l). Digit l of the output
   * has rs(l) bits, from the most significant digit (l = 0) to the least significant one; the corresponding digits of
   * the input are taken from the least significant one. For a uniform radix, this is Rmat(r, n).
   *
   * Step m (applied in increasing order of m) rotates the n - s_m least significant bits (s_m = rs(0) + ... + rs(m - 1))
   * so that input digit m (then at the bottom) lands just below the digits already placed.
   */
  def Rmat(n: Int, rs: Seq[Int]): Matrix[F2] =
    require(rs.nonEmpty && rs.forall(_ > 0) && rs.sum == n, s"radices ($rs) must be strictly positive and sum up to n ($n)")
    val s = rs.scanLeft(0)(_ + _)
    rs.indices.map(m => Matrix.identity[F2](s(m)) oplus Lmat(rs(m), n - s(m))).reverse.reduceLeft(_ * _)

  def Lmat(m: Int, n: Int): Matrix[F2] = Cmat(n) ^ (n - m)

  def Cmat(n: Int): Matrix[F2] = Matrix.tabulate[F2](n, n)((i, j) => F2((i + 1) % n == j))

  /** Permutation preceding stage l of a uniform radix-2^r Cooley-Tukey FFT. */
  def Qmat(n: Int, r: Int, l: Int): Matrix[F2] = Qmat(n, Seq.fill(n / r)(r), l)

  /**
   * Permutation preceding stage l of a mixed-radix Cooley-Tukey FFT with radices 2^rs(l), stage 0 being the leftmost
   * factor (i.e. the last one applied).
   *
   * Before this permutation, the data consists (from the most significant bits) of the s_l group bits, the frequency index
   * of the sub-transforms computed so far, and the rs(l + 1) bits output by the butterflies of the previous stage. The
   * second factor rotates the latter to the top of the frequency index; the first factor rotates the lowest group digit
   * (the rs(l) bits to be combined by this stage) down to the butterfly position.
   */
  def Qmat(n: Int, rs: Seq[Int], l: Int): Matrix[F2] =
    val s = rs.take(l).sum
    val r = rs(l)
    val c = n - s - r // bits of the sub-transforms computed before this stage
    val rNext = if l + 1 < rs.size then rs(l + 1) else 0
    val mat1 = Matrix.identity[F2](s) oplus Lmat(c, n - s)
    val mat2 = Matrix.identity[F2](s + r) oplus Lmat(rNext, c)
    mat1 * mat2

  def stream[T](matrices: Seq[Matrix[F2]], k: Int, hw: HW[T], control:RAMControl): StreamingModule[T] = LinearPerm[T](matrices).stream(k,control)(using hw)
