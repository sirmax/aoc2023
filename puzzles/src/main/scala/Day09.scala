import cats.data.NonEmptyList
import cats.syntax.all.*
import kyo.>
import kyo.apps.App.Effects
import kyo.tries.Tries

import scala.collection.immutable.Queue

object Day09 extends util.AocApp(2023, 9) {
  case class Input(histories: NonEmptyList[History])

  case class History(values: NonEmptyList[Long])

  def parseInput(s: String): Input > Effects = {
    import cats.parse.{Numbers as N, Rfc5234 as R, Parser as P}
    import cats.syntax.all.*

    val sps    = R.sp.rep.void
    val slong  = N.signedIntString.map(_.toLong).surroundedBy(R.sp.rep0)
    val slongs = slong.rep
    val nl     = P.char('\n')

    val history = slongs.map(History.apply)
    val input   = (history.repSep(nl) <* nl.?).map(Input.apply)

    val result = input.parseAll(s)
    result
      .leftMap(e => s"$e ${e.input.map(s => s.slice(e.failedAtOffset, e.failedAtOffset + 32))}")
      .fold(Tries.fail, identity)
  }

  def part1(input: Input): String > Effects = {
    val result = input.histories.map { h =>
      Vector
        .unfold(h.values.toList) { h =>
          Option.when(h.exists(_ != 0)) { h -> h.sliding2.map((x, y) => y - x) }
        }
        .foldRight(0L) { (values, x) => values.last + x }
    }.sumAll
    s"$result"
  }

  def part2(input: Input): String > Effects = {
    val result = input.histories.map { h =>
      Vector
        .unfold(h.values.toList) { h =>
          Option.when(h.exists(_ != 0)) { h -> h.sliding2.map((x, y) => y - x) }
        }
        .foldRight(0L) { (values, x) => values.head - x }
    }.sumAll
    s"$result"
  }
}
