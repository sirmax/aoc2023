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
    val result = "???"
    s"$result"
  }
}
