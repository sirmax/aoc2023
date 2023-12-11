import cats.data.NonEmptyList
import cats.syntax.all.*
import kyo.>
import kyo.apps.App.Effects
import kyo.tries.Tries

object Day6 extends util.AocApp(2023, 6) {
  case class Input(times: NonEmptyList[Long], distances: NonEmptyList[Long]) {
    val races: NonEmptyList[Race] = (times zip distances).map(Race.apply)
  }

  case class Race(time: Long, distance: Long)

  def parseInput(s: String): Input > Effects = {
    import cats.parse.{Numbers => N, Parser => P, Rfc5234 => R}
    import cats.parse.Rfc5234
    import cats.syntax.all.*

    val sps   = R.sp.rep.void
    val long  = N.digits.map(_.toLong).surroundedBy(R.sp.rep0)
    val longs = long.rep
    val nl    = P.char('\n')

    val time     = P.string("Time:") *> longs
    val distance = P.string("Distance:") *> longs
    val input    = (time, nl, distance, nl.?).mapN((time, _, distance, _) => Input(time, distance))
    val result   = input.parseAll(s)

    result
      .leftMap(e => s"$e ${e.input.map(s => s.slice(e.failedAtOffset, e.failedAtOffset + 32))}")
      .fold(Tries.fail, identity)
  }

  def part1(input: Input): String > Effects = {
    val result = input.races.map(solve).toList.product
    s"$result"
  }

  def part2(input: Input): String > Effects = {
    val time     = input.times.mkString_("").toLong
    val distance = input.distances.mkString_("").toLong
    val race     = Race(time, distance)
    val result   = solve(race)
    s"$result"
  }

  private def solve(r: Race) = {
    // x -- time held button == speed
    // x * (r.time - x) >= r.distance + 1
    // -x^2 + r.time * x + (-r.distance - 1) >= 0
    util.Math.solveQuadraticEquation(a = -1, b = r.time, c = -r.distance - 1).sorted match {
      case List(x1, x2) => (x2.floor - x1.ceil).toLong + 1
      case List(_)      => 1
      case _            => 0
    }
  }
}
