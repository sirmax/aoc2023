import cats.Show
import cats.data.NonEmptyList
import cats.syntax.all.*
import kyo.>
import kyo.apps.App.Effects
import kyo.tries.Tries

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

    def foo(name: String, vel: Stone => BigDecimal, ord: Stone => BigDecimal) = {
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

      def ordsT(t: Int) = input.hail.iterator.map(s => ord(s) + t * vel(s))

      vs.toList
        .map { v =>

          // vs - v = dv
          // dv < 0  -- stone below hail -- hail above stone
          // dv > 0  -- stone above hail -- hail below stone
          // dv == 0 -- exact match

          val o_v_dv = input.hail
            // .map(s => (ord(s), vel(s), spire.math.lcm(vel(s).toBigInt.abs, v.toBigInt.abs)))
            .map(s => (s, ord(s), vel(s), vel(s) - v))
            .sortBy((_, _, _, dv) => dv)
          // .tapEach { (s, ordS, velS, dv) =>
          //   println(s"$name: ord(s)=$ordS vel(s)=$velS dv=$dv")
          // }
          val hailAboveStone = o_v_dv.filter((_, _, _, dv) => dv < 0)
          val hailBelowStone = o_v_dv.filter((_, _, _, dv) => dv > 0)

          val hailAboveAtT0 = hailAboveStone.map((s, _, _, _) => s).toSet
          val hailBelowAtT0 = hailBelowStone.map((s, _, _, _) => s).toSet

          // println(
          //   s"$name: stone in (${hailBelowStone.maxByOption((_, o, _, _) => o)}, ${hailAboveStone
          //       .minByOption((_, o, _, _)                                => o)})",
          // )

          def hailAtT(t: BigDecimal) = {
            input.hail
              .map(s => s -> (ord(s) + vel(s) * t))
              .sortBy(_._2)
              // now we have hailstones at time=t, ordered by their ordinate from lowest to highest
              .sliding2
              .find {
                case ((s1, o1), (s2, o2)) =>
                  (hailBelowAtT0(s1) && hailAboveAtT0(s2) && o1 > o2) ||
                  (hailBelowAtT0(s2) && hailAboveAtT0(s1) && o2 > o1)
              }
          }

          // println(s"$name: hailAtT(0)=${hailAtT(0)}")
          // println(s"$name: hailAtT(input.testArea.min)=${hailAtT(input.testArea.min)}")
          // println(s"$name: hailAtT(input.testArea.max)=${hailAtT(input.testArea.max)}")

          // println(s"$name: find hailAtT(?)=${LazyList.iterate(BigDecimal(1))(_ * 2).collectFirstSome(hailAtT)}")

          def binSearch(i0: BigInt, test: BigInt => Int): Either[BigInt, BigInt] = {
            // NOTE: not matching test=0 in min bounds to avoid hitting one of possible many 0 results.
            val minBound =
              i0 + LazyList.iterate(BigInt(1))(_ * 2).takeWhile(t => test(i0 + t) < 0).lastOption.getOrElse(i0)
            val maxBound = minBound + LazyList.iterate(BigInt(1))(_ * 2).find(t => test(minBound + t) >= 0).head
            println(s"$name: i0=$i0 minBound=$minBound maxBound=$maxBound")
            @tailrec def recur(min: BigInt, max: BigInt): Either[BigInt, BigInt] = {
              val mid = min + (max - min) / 2
              if (mid == min) {
                if (test(max) == 0) max.asRight else max.asLeft
              } else {
                test(mid) match {
                  case 0 => mid.asRight
                  case i => if (i > 0) recur(min, mid) else recur(mid, max)
                }
              }
            }
            recur(minBound, maxBound)
          }

          def binSearchBD(i0: BigDecimal, test: BigDecimal => Int): Either[BigDecimal, BigDecimal] =
            binSearch(i0.toBigInt, t => test(BigDecimal(t))).bimap(BigDecimal.apply, BigDecimal.apply)

          def timeOfFirstIntersection(ps: BigDecimal): BigDecimal = {
            // x = p + v*t  ->  ps + vs*t = pi + vi*t  -> t = (pi - ps) / (vs - vi)
            input.hail.iterator.flatMap(s => ((ord(s) - ps) / (v - vel(s))).some.filter(_ > 0)).minOption.getOrElse(0)
          }

          // NOTE: May yield multiple 0, for example on "sample" X axis both 28 and 32 work
          def test(ps: BigDecimal): Int = {
            val intersections = input.hail.iterator.map { s =>
              val t          = (ord(s) - ps) / (v - vel(s))
              val tInt       = BigDecimal(t.toBigInt)
              val x          = ord(s) + vel(s) * t
              val xTInt      = ord(s) + vel(s) * tInt
              val xStone     = ps + v * t
              val xStoneTInt = ps + v * tInt

              val res =
                if (t < 0) v.signum
                else if (x <= 0) 1
                else if (t == tInt && x == xStone) 0
                else if ((hailAboveAtT0(s) && xTInt < xStoneTInt) || (!hailAboveAtT0(s) && xTInt > xStoneTInt)) 1
                else -1
              println(
                s"$name test($ps, $v, ${ord(s)}, ${vel(s)})=$res: t=$t ($tInt) x=$x ($xTInt) xStone=$xStone ($xStoneTInt) hailAboveAtT0(s)=${hailAboveAtT0(s)}",
              )
              res
            }
            val res = LazyList
              .from(intersections)
              .scanLeft(0) {
                case (1, _) => 1
                case (0, c) => c
                case (_, 0) => -1
                case (_, c) => c
              }
              .lastOption
              .getOrElse(0)
            println(s"$name test($ps, $v)=$res")
            res
          }

          val huh = binSearchBD(hailBelowAtT0.maxByOption(ord).map(ord).getOrElse(0), test)
          // val huh = binSearchBD(0, test)
          println(s"$name: huh=$huh")
          // val tHailSidesFlip = binSearchBD(t => hailAtT(t).as(1).getOrElse(-1))
          // println(s"$name: tHailSidesFlip=$tHailSidesFlip hailAtT(t)=${hailAtT(tHailSidesFlip.merge)}")

          // .groupBy((_, _, dv) => dv)
          // .tapEach { (dv, group) =>
          //   if (group.size > 1) { println(s"$name: lcmV=$lcmV group=$group")}
          // }

          // println(s"$name: v=$v ${spire.math.prime.factor(v.toBigInt)}")
          // val divisors = integerDivisors(v)
          // val divisors = List.iterate(BigDecimal(0), 100 * v.abs.toInt + 1)(_ + 1 )
          val t = 1
          if (v > 0) {
            val base = ordsT(t).min
            v -> (base - t * v)
          } else if (v < 0) {
            val base = ordsT(t).max
            v -> (base - t * v)
          } else {
            sys.error(s"I'm not ready for 0 velocity!")
          }
        }
        .mapFilter { (v, p) =>
          input.hail
            // .tapEach(s => println(s"mapFilter($v, $p): $s (vel(s) - v) = (${vel(s)} - $v} = ${vel(s) - v}"))
            .fproductLeft { s =>
              (vel(s) - v).some.filter(_ != 0).map((p - ord(s)) / _).orElse(BigDecimal(0).some).filter(_ % 1 == 0)
            }
            .some
            .mapFilter(x =>
              Option.when(x.forall((t, _) => t.isDefined))(x.map((t, s) => t.get -> s).sortBy((t, _) => t)),
            )
            .tupleLeft((v, p))
        }
    }

    val x = foo("x", _.vx, _.px)
    println(s"x: $x")
    val y = foo("y", _.vy, _.py)
    println(s"y: $y")
    val z = foo("z", _.vz, _.pz)
    println(s"z: $z")

    val result = (x.map(_._1._2), y.map(_._1._2), z.map(_._1._2))
      .mapN(_ + _ + _)
      .tapEach(println)
      .head

    // assert(result > 625890197021615L, s"FALSE: $result > 625890197021615L")
    s"$result"
  }

  private def integerDivisors(d: BigDecimal): List[BigDecimal] = {
    val factors = spire.math.prime.factor(d.toBigInt)
    val xxx = factors.toList
      .flatMap((f, n) => List.fill(n)(f.toBigDecimal))
    BigDecimal(1) :: List.range(1, xxx.size + 1).flatMap(n => xxx.combinations(n).map(_.product))
  }
}
