import cats.data.NonEmptyList
import cats.syntax.all.*
import kyo.>
import kyo.App.Effects
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
    def solveSqEq(r: Race): Option[(Double, Double) | Double] = {
      // x -- time held button == speed
      // x * (r.time - x) >= r.distance + 1
      // -x^2 + r.time * x + (-r.distance - 1) >= 0

      val a = -1
      val b = r.time.toDouble
      val c = (-r.distance - 1).toDouble
      val d = (b * b) - (4 * a * c)
      d match {
        case 0 =>
          Some(-b / 2 * a)
        case d if d > 0 =>
          // (-b +- sqrt(d)) / 2a
          val sd = Math.sqrt(d)
          val x1 = (-b + sd) / (2 * a)
          val x2 = (-b - sd) / (2 * a)
          Some((x1, x2))
        case _ => None
      }
    }

    solveSqEq(r) match {
      case Some((x1, x2)) => (x2.floor - x1.ceil).toLong + 1
      case Some(_)        => 1
      case None           => 0
    }
  }
}
