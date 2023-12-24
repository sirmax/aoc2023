import cats.Show
import cats.data.NonEmptyList
import cats.syntax.all.*
import kyo.>
import kyo.apps.App.Effects
import kyo.tries.Tries

object Day24 extends util.AocApp(2023, 24) {
  case class Input(hail: List[Stone], testArea: TestArea)

  case class Stone(px: Long, py: Long, pz: Long, vx: Long, vy: Long, vz: Long)

  case class TestArea(min: Long, max: Long)

  object Input {
    given Show[Input] = i => s"Hail:\n  ${i.hail.mkString("\n  ")}\n${i.testArea}"
  }

  def parseInput(s: String): Input > Effects = {
    import cats.parse.{Numbers as N, Rfc5234 as R, Parser as P}

    val long    = N.signedIntString.map(_.toLong)
    val longSep = long <* P.charIn(",@ ").rep0.?
    val stone   = (longSep, longSep, longSep, longSep, longSep, long).mapN(Stone.apply)
    val hail    = stone.repSep(R.lf).map(_.toList)

    val testArea = P.string("test area ") *> (longSep, longSep).mapN(TestArea.apply)

    val input = (hail <* R.lf.rep(2), testArea <* R.lf.?).mapN(Input.apply)

    input.parseAll(s).leftMap(e => throw new IllegalArgumentException(s"Failed to parse\n${e.show}")).merge
  }

  def part1(input: Input): String > Effects = {
    // println(input.show)
    val result = "???"
    s"$result"
  }

  def part2(input: Input): String > Effects = {
    val result = "???"
    s"$result"
  }
}
