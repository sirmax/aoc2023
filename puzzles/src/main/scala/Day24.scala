import cats.Show
import cats.data.NonEmptyList
import cats.syntax.all.*
import kyo.>
import kyo.apps.App.Effects
import kyo.tries.Tries

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

    println(s"x: ${parallel(_.vx, _.px)}")
    println(s"y: ${parallel(_.vy, _.py)}")
    println(s"z: ${parallel(_.vz, _.pz)}")

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
                divisors.flatMap(t => List(v.sign * (d / t + v.abs), -v.sign * (d / t - v.abs)))
                  // technically, 0 is a viable value, at least for x and y, but we're not ready for it.
                  .filter(_ != 0)
              // println(s"d=$d v=$v factors=$factors divisors=$divisors vs=$vs")
              vs.toSet
            }
            .reduce(_ intersect _)
        }
        .reduce(_ intersect _)

      vs.toList
        .flatMap { v =>
          // println(s"$name: v=$v ${spire.math.prime.factor(v.toBigInt)}")
          val divisors = integerDivisors(v)
          // val divisors = List.iterate(BigDecimal(0), 100 * v.abs.toInt + 1)(_ + 1 )
          val ordsT1 = input.hail.iterator.map(s => ord(s) + vel(s))
          if (v > 0) {
            val base = ordsT1.min
            divisors.map(d => v -> (base - v - d))
          }
          else if (v < 0) {
            val base = ordsT1.max
            divisors.map(d => v -> (base - v + d))
          }
          else {
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
    val y = foo("y", _.vy, _.py)
    val z = foo("z", _.vz, _.pz)

    println(s"x: $x")
    println(s"y: $y")
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
