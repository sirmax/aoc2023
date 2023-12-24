import cats.Show
import cats.data.NonEmptyList
import cats.syntax.all.*
import kyo.>
import kyo.apps.App.Effects
import kyo.tries.Tries

object Day24 extends util.AocApp(2023, 24) {
  case class Input()

  object Input {
    given Show[Input] = input => "Input()"
  }

  def parseInput(s: String): Input > Effects = {
    import cats.parse.{Numbers as N, Rfc5234 as R, Parser as P}

    val input = (P.anyChar.rep0.string <* R.lf.?).map { _ =>
      Input()
    }

    input.parseAll(s).leftMap(e => throw new IllegalArgumentException(s"Failed to parse\n${e.show}")).merge  }

  def part1(input: Input): String > Effects = {
    println(input.show)
    val result = "???"
    s"$result"
  }

  def part2(input: Input): String > Effects = {
    val result = "???"
    s"$result"
  }
}
