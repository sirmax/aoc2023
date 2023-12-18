import cats.Show
import cats.data.NonEmptyList
import cats.syntax.all.*
import kyo.>
import kyo.apps.App.Effects
import kyo.tries.Tries

import scala.collection.immutable.BitSet

object Day18 extends util.AocApp(2023, 18) {
  case class Input(plan: List[Instruction])

  object Input {
    given Show[Input] = _.plan.mkString_("\n")
  }

  case class Instruction(digDirection: DigDirection, length: Long, color: Color)

  object Instruction {
    given Show[Instruction] = i => s"${i.digDirection.show} ${i.length.show} ${i.color.show}"
  }

  enum DigDirection:
    case U, R, D, L // Matching Direction.NESW order just in case

  object DigDirection {
    given Show[DigDirection] = _.toString
  }

  case class Color(rgb: Int)

  object Color {
    given Show[Color] = c => f"(#${c.rgb}%06x)"
  }

  def parseInput(s: String): Input > Effects = {
    import cats.parse.{Numbers as N, Rfc5234 as R, Parser as P}

    val dir = DigDirection.values.map(d => P.string(d.toString).as(d)).reduce(_ | _)

    val len = R.digit.rep.string.map(_.toInt)

    val color =
      P.string("(#") *> R.hexdig.rep(6).string.map(s => Integer.parseInt(s, 16)).map(Color.apply) <* P.char(')')

    val instruction = (dir, R.wsp, len, R.wsp, color).mapN((d, _, l, _, c) => Instruction(d, l, c))

    val input = (instruction.repSep(R.lf) <* R.lf.?).map(_.toList).map(Input.apply)

    input
      .parseAll(s)
      .leftMap(e => s"$e ${e.input.map(s => s.slice(e.failedAtOffset, e.failedAtOffset + 32))}")
      .fold(Tries.fail, identity)
  }

  def part1(input: Input): String > Effects = {
    val result = solve(input.plan)
    s"$result"
  }

  def part2(input: Input): String > Effects = {
    val plan = input.plan.map { i =>
      // Each hexadecimal code is six hexadecimal digits long.
      // The first five hexadecimal digits encode the distance in meters as a five-digit hexadecimal number.
      // The last hexadecimal digit encodes the direction to dig:
      // 0 means R, 1 means D, 2 means L, and 3 means U.
      val l = i.color.rgb >> 4
      val d = (i.color.rgb & 3) match {
        case 0 => DigDirection.R
        case 1 => DigDirection.D
        case 2 => DigDirection.L
        case 3 => DigDirection.U
      }
      Instruction(d, l, i.color)
    }
    val result = solve(plan)
    s"$result"
  }

  private def solve(plan: List[Instruction]) = {
    val rawCoords = plan.scanLeft((0L, 0L, 0L)) {
      case ((x, y, l), i) =>
        i.digDirection match {
          case DigDirection.U => (x, y - i.length, i.length)
          case DigDirection.R => (x + i.length, y, i.length)
          case DigDirection.D => (x, y + i.length, i.length)
          case DigDirection.L => (x - i.length, y, i.length)
        }
    }

    // Shoelace formula + Pick's theorem, just like in Day10.
    val (area2, nOutside) = rawCoords.sliding2
      .foldLeft((0L, 0L)) {
        case ((a, l), ((x1, y1, _), (x2, y2, l2))) => (a + (x2 + x1) * (y2 - y1), l + l2)
      }

    val area    = area2.abs / 2
    val nInside = area + 1 - nOutside / 2

    nOutside + nInside
  }
}
