import cats.Show
import cats.data.NonEmptyList
import cats.syntax.all.*
import kyo.>
import kyo.apps.App.Effects
import kyo.tries.Tries
import spire.math.{SafeLong, lcm}
import spire.syntax.all.*

import java.math.MathContext
import scala.annotation.tailrec

object Day24 extends util.AocApp(2023, 24) {
  case class Input(hail: List[Stone], testArea: TestArea)

  case class Stone(px: BigDecimal, py: BigDecimal, pz: BigDecimal, vx: BigDecimal, vy: BigDecimal, vz: BigDecimal)

  case class TestArea(min: BigDecimal, max: BigDecimal)

  object Input {
    given Show[Input] = i => s"Hail:\n  ${i.hail.mkString("\n  ")}\n${i.testArea}"
  }

  def parseInput(s: String): Input > Effects = {
    import cats.parse.{Numbers as N, Rfc5234 as R, Parser as P}

    val n       = N.signedIntString.map(BigDecimal(_))
    val longSep = n <* P.charIn(",@ ").rep0.?
    val stone   = (longSep, longSep, longSep, longSep, longSep, n).mapN(Stone.apply)
    val hail    = stone.repSep(R.lf).map(_.toList)

    val testArea = P.string("test area ") *> (longSep, longSep).mapN(TestArea.apply)

    val input = (hail <* R.lf.rep(2), testArea <* R.lf.?).mapN(Input.apply)

    input.parseAll(s).leftMap(e => throw new IllegalArgumentException(s"Failed to parse\n${e.show}")).merge
  }

  def part1(input: Input): String > Effects = {
    // println(input.show)

    def cross(s1: Stone, s2: Stone) = {
      // x = ((vy1 * vx2 * px1) + (vx1 * vx2 * py2) - (vx1 * vx2 * py1) - (vx1 * vy2 * px2)) / (vy1 * vx2 - vx1 * vy2)
      // format: off
      val x = (s1.vy * s2.vx - s1.vx * s2.vy).some.filter(_ != 0).map(d => ((s1.vy * s2.vx * s1.px) + (s1.vx * s2.vx * s2.py) - (s1.vx * s2.vx * s1.py) - (s1.vx * s2.vy * s2.px)) / d)
      val y = (s1.vx * s2.vy - s1.vy * s2.vx).some.filter(_ != 0).map(d => ((s1.vx * s2.vy * s1.py) + (s1.vy * s2.vy * s2.px) - (s1.vy * s2.vy * s1.px) - (s1.vy * s2.vx * s2.py)) / d)
      // format: on
      (x, y).tupled
    }
    val result = input.hail.combinations(2).count { s1_s2 =>
      val List(s1, s2) = s1_s2
      val result = cross(s1, s2).exists { (x, y) =>
        def inTestArea =
          x >= input.testArea.min && x <= input.testArea.max && y >= input.testArea.min && y <= input.testArea.max

        def inFuture =
          (s1.vx.sign == (x - s1.px).sign) && (s1.vy.sign == (y - s1.py).sign) &&
            (s2.vx.sign == (x - s2.px).sign) && (s2.vy.sign == (y - s2.py).sign)

        inTestArea && inFuture
      }
      result
    }
    s"$result"
  }

  def part2(input: Input): String > Effects = {
    def parallel(v: Stone => BigDecimal, ord: Stone => BigDecimal) =
      input.hail
        .groupBy(v)
        .filter(_._2.size > 1)
        .map((v, stones) => v -> stones.map(ord))
        .toList
        .sortBy(x => (x._1, x._2.size))

    // println(s"x: ${parallel(_.vx, _.px)}")
    // println(s"y: ${parallel(_.vy, _.py)}")
    // println(s"z: ${parallel(_.vz, _.pz)}")

    def foo(name: String, vel: Stone => BigDecimal, ord: Stone => BigDecimal): Option[SafeLong] = {
      val groups = parallel(vel, ord)
      val vs = groups
        .map { (v, ords) =>
          ords
            .combinations(2)
            .map { pair =>
              val List(o1, o2) = pair
              val d            = (o1 - o2).abs
              // v(stone) = (d ± v*t) / t
              // same direction:
              //   v(stone) = (d + v*t) / t
              //            = d/t + v
              val divisors = integerDivisors(d)
              val vs =
                divisors
                  .flatMap(t => List(v.sign * (d / t + v.abs), -v.sign * (d / t - v.abs)))
                  // technically, 0 is a viable value, at least for x and y, but we're not ready for it.
                  .filter(_ != 0)
                  .map(_.toBigInt)
              // println(s"d=$d v=$v divisors=$divisors vs=$vs")
              vs.toSet
            }
            .reduce(_ intersect _)
        }
        .reduce(_ intersect _)

      // x = p + v*t; t = (x - p)/v
      // ord(s) + vel(s) * t = p + v*t
      // ord(s) + (vel(s) - v)*t(s) - p = 0
      //
      // (x - p)/v = (x - ord(s))/vel(s)
      // x - p = (x - ord(s))*v/vel(s)
      // x*vel(s) - p*vel(s) = x*v - ord(s)*v
      // x*(vel(s) - v) - p*vel(s) + ord(s)*v = 0

      val pvs = input.hail.map(s => PV(p = ord(s).toBigInt, v = vel(s).toBigInt))

      def ordsT(t: Int) = LazyList.from(input.hail).map(s => ord(s) + t * vel(s))
      def vels = List.from(input.hail).map(s => vel(s))

      println(s"$name possible vs=${vs.toList.sorted}")
      vs.toList
        .collectFirstSome { v =>

          // vs - v = dv
          // dv < 0  -- stone below hail -- hail above stone
          // dv > 0  -- stone above hail -- hail below stone
          // dv == 0 -- exact match

          val pvsAdjusted = pvs.map(pv => pv.copy(v = pv.v - v))
          println(s"$name v=$v pvsAdjusted=$pvsAdjusted pvs=$pvs")

          val sameVelocity = pvsAdjusted.collectFirstSome(_.some.filter(_.v == 0))

          sameVelocity match {
            case Some(pv) =>
              // x = p + v*t
              // p + v*t = pv.p + pv.v*t
              // t = (p - pv.p) / (pv.v - v)
              val pv0 = PV(pv.p, v)
              def allInExpectedBounds = pvs.forall {
                case `pv0` => true
                case pv =>
                  val t = (pv0.p - pv.p) / (pv.v - pv0.v)
                  def inFuture = t > 0
                  def xPositive = pv.p + t * pv.v > 0
                  inFuture && xPositive
              }
              println(s"$name v=$v ZERO CASE: allInExpectedBounds=$allInExpectedBounds")

              Option.when(allInExpectedBounds)(pv.p)

            case None =>
              val pvCommon =
                pvsAdjusted.sortBy(pv => pv.p % pv.v).map(_.some).reduce((pv1, pv2) => (pv1, pv2).flatMapN(combine))

              val pv = pvCommon
                .flatMap { pvCommon =>
                  val (rising, falling) = pvsAdjusted.partition(_.v > 0)
                  val pMin              = rising.map(pv => pv.p + pv.v).maxOption
                  val pMax              = falling.map(pv => pv.p + pv.v).minOption

                  // This "filter" does not take the negative collision ordinates into account,
                  // but it's good enough for solving the main task, which has no alternative
                  // velocities.
                  val p = (pMin, pMax) match {
                    case (min, Some(max)) =>
                      val p = ((max - pvCommon.p) / pvCommon.v) * pvCommon.v + pvCommon.p
                      if (min.forall(_ <= p)) p.some else None

                    case (Some(min), None) =>
                      val (n, rem) = (min - pvCommon.p) /% pvCommon.v
                      if (rem == 0) min.some else (n * pvCommon.v.abs + pvCommon.p).some

                    case (None, None) =>
                      None
                  }
                  println(s"pMin=$pMin pMax=$pMax pvCommon=$pvCommon p=$p")
                  p.map(PV(_, v))
                }
                .filter(_.p > 0)
              println(s"$name v=$v pv=$pv pvCommon=$pvCommon")
              pv.map(_.p)
          }
        }
    }

    val x = foo("x", _.vx, _.px)
    println(s"x: $x")
    val y = foo("y", _.vy, _.py)
    println(s"y: $y")
    val z = foo("z", _.vz, _.pz)
    println(s"z: $z")

    val result = (x, y, z)
      .mapN(_ + _ + _)
      .tapEach(println)
      .headOption

    // assert(result > 625890197021615L, s"FALSE: $result > 625890197021615L")
    s"$result"
  }

  private def integerDivisors(d: BigDecimal): List[BigDecimal] = {
    val factors = spire.math.prime.factor(d.toBigInt)
    val xxx = factors.toList
      .flatMap((f, n) => List.fill(n)(f.toBigDecimal))
    BigDecimal(1) :: List.range(1, xxx.size + 1).flatMap(n => xxx.combinations(n).map(_.product))
  }

  /** I don't have a better name for it at the moment.
    * It takes two lines (in `PV` form) and, if that is possible, returns another line,
    * that has integer (t, x) where both lines have integer (t, x).
    */
  private def combine(pv1: PV, pv2: PV): Option[PV] = {
    val res = _combine(pv1, pv2)
    println(s"combine($pv1, $pv2) = $res")
    res
  }

  private def _combine(pv1: PV, pv2: PV): Option[PV] = {
    val r1 = pv1.p % pv1.v
    val r2 = pv2.p % pv2.v

    if (r1 == r2) {
      PV(r1, pv1.v lcm pv2.v).some
    } else { // this condition is too restrictive !!!
      // N == pv1.v * k + pv1.p == pv2.v * m + pv2.p
      // pv1.v * k + pv1.p - pv2.p == pv2.v * m
      // (pv1.v * k + pv1.p - pv2.p) % pv2.v == 0
      LazyList
        .from(1)
        .map(pv1.v * _ + pv1.p)
        .take(10_000_000)
        .find(n => (n - pv2.p) % pv2.v == 0)
        .map(N => PV(p = N, v = pv1.v lcm pv2.v))
    }
  }

  private case class PV(p: SafeLong, v: SafeLong)
}
