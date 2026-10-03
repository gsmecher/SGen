package transforms.perm

import ir.rtl.RAMControl
import ir.rtl.hardwaretype.{HW, Unsigned}
import maths.fields.F2
import maths.linalg.Matrix
import org.scalatest.Tag
import org.scalatest.funsuite.AnyFunSuiteLike
import helpers.Xsim
import helpers.Xsim.test

class LinearPermTest extends AnyFunSuiteLike:
  val hw: HW[Int] = Unsigned(16)

  for
    t <- 1 to 5
    k <- 1 to 5
    n = t + k
    m <- 0 until n
    dp <- RAMControl.values
  do
    test(s"Full-shuffle simulation (size 2^$n, power $m, 2^$k ports, $dp RAM banks)", Tag("simulation")):
      val inputs = 0 until (1 << n) * 5
      LinearPerm(LinearPerm.Lmat(m, n)).test(k, dp, hw, s"shuffle-$n-$m-$k-$dp")

  for
    t <- 1 to 5
    k <- 1 to 5
    n = t + k
    r <- 1 until n if n % r == 0
    dp <- RAMControl.values
  do
    test(s"Bitreversal simulation (size 2^$n, blocks 2^$r, 2^$k ports, $dp RAM banks)", Tag("simulation")):
      val inputs = 0 until (1 << n) * 5
      LinearPerm(LinearPerm.Rmat(r, n)).test(k, dp, hw, s"bitrev-$n-$r-$k-$dp")

  // The mixed-radix digit reversal and Cooley-Tukey stage permutations reduce to the original uniform-radix formulas.
  for
    n <- 1 to 12
    r <- 1 to n if n % r == 0
  do
    test(s"Uniform-radix Rmat and Qmat (size 2^$n, radix 2^$r) match the mixed-radix definitions"):
      import LinearPerm.Lmat
      val legacyR = (0 until n / r).map(l => Matrix.identity[F2](n - r * (l + 1)) oplus Lmat(r, r * (l + 1))).reduceLeft(_ * _)
      assert(LinearPerm.Rmat(r, n) == legacyR)
      for l <- 0 until n / r do
        val legacyQ = (Matrix.identity[F2](r * l) oplus Lmat(n - r * (l + 1), n - r * l)) * (Matrix.identity[F2](r * (l + 1)) oplus Lmat(r, n - r * (l + 1)))
        assert(LinearPerm.Qmat(n, r, l) == legacyQ)

  // Mixed-radix digit reversal: digit l of the output (rs(l) bits, from the most significant one) is digit l of the input counted from the least significant one.
  for rs <- Seq(Seq(1, 2), Seq(2, 1), Seq(3, 1), Seq(1, 3), Seq(3, 3, 1), Seq(1, 3, 3), Seq(2, 1, 3), Seq(3, 2, 1)) do
    val n = rs.sum
    test(s"Mixed-radix digit reversal (size 2^$n, digits ${rs.mkString(",")})"):
      val P = LinearPerm.Rmat(n, rs)
      val starts = rs.scanLeft(0)(_ + _)
      for i <- 0 until 1 << n do
        val digits = rs.indices.map(l => (i >> starts(l)) % (1 << rs(l))) // digit l of the input, from the least significant one
        val expected = rs.indices.foldLeft(0)((acc, l) => (acc << rs(l)) + digits(l))
        assert(LinearPerm.permute(P, i) == expected, s"i=$i")




  // Temporal permutations whose top r time bits are fixed: the memory banks are then folded 2^r times, and the control
  // sequences are indexed by dataset and fold. The remaining time bits are cyclically shifted (a period of c datasets),
  // optionally with a contribution of the port bits, so that the periods of the control sequences are not multiples of 2^r.
  for
    r <- 1 to 3
    c <- 2 to 3
    t = r + c
    k <- 1 to 2
    n = t + k
    withOffset <- Seq(false, true)
    dp <- RAMControl.values
  do
    val rows = (0 until n).map(i =>
      val cols = if i >= r && i < t then Set(r + (i - r + 1) % c) ++ (if withOffset then Set(t + (i - r) % k) else Set()) else Set(i)
      (0 until n).map(j => if cols(j) then '1' else '.').mkString)
    val P = maths.linalg.Matrix(n, n, rows.mkString)
    test(s"Folded temporal permutation simulation (size 2^$n, $r fixed bits, period $c, offset $withOffset, 2^$k ports, $dp RAM banks)", Tag("simulation")):
      LinearPerm(P).test(k, dp, hw, s"folded-$n-$r-$c-$k-$withOffset-$dp")
