import cats.data.NonEmptyList
import cats.syntax.all.*
import kyo.>
import kyo.apps.App.Effects
import kyo.tries.Tries

object Day11 extends util.AocApp(2023, 11) {
  case class Input(coords: NonEmptyList[Coord])

  case class Coord(r: Int, c: Int) {
    override def toString: String = s"($r, $c)"
  }

  def parseInput(s: String): Input > Effects = {
    import cats.parse.{Numbers as N, Rfc5234 as R, Parser as P}
    import cats.syntax.all.*

    val nl = P.char('\n')

    val galaxyCoord = (P.char('#') *> P.caret).map { c =>
      // col and row are handled differently because I'm lazy to figure out how to put P.caret before P.char.
      // using 1-based indexing because it's makes easier to find the coords in text editor
      Coord(r = c.line + 1, c = c.col)
    }
    val coords = galaxyCoord.surroundedBy((P.char('.') | nl).rep0).rep

    val input = coords.map(Input.apply)

    val result = input.parseAll(s)

    result
      .leftMap(e => s"$e ${e.input.map(s => s.slice(e.failedAtOffset, e.failedAtOffset + 32))}")
      .fold(Tries.fail, identity)
  }

  def part1(input: Input): String > Effects = {
    val result = solve(input, expand = 2L)
    s"$result"
  }

  def part2(input: Input): String > Effects = {
    val result = solve(input, expand = 1000000L)
    s"$result"
  }

  private def solve(input: Input, expand: Long): Long = {
    //    println(input)
    def emptyLines(f: Coord => Int) = {
      val nonEmpty = input.coords.toList.map(f).toSet
      (nonEmpty.min to nonEmpty.max).filterNot(nonEmpty).toSet
    }

    val emptyRows = emptyLines(_.r)
    val emptyCols = emptyLines(_.c)
    //    println(s"emptyRows=${emptyRows.toList.sorted}, emptyCols=${emptyCols.toList.sorted}")

    def minMax(a: Int, b: Int) = if (a < b) (a, b) else (b, a)

    val result = input.coords.toList
      .combinations(2)
      .map {
        case List(c1, c2) =>
          val (rMin, rMax) = minMax(c1.r, c2.r)
          val (cMin, cMax) = minMax(c1.c, c2.c)
          val rDist        = rMax - rMin + (rMin + 1 until rMax).count(emptyRows).toLong * (expand - 1)
          val cDist        = cMax - cMin + (cMin + 1 until cMax).count(emptyCols).toLong * (expand - 1)
          val d            = rDist + cDist
          //        println(s"($c1, $c2): $d; r: ${rMax-rMin} + ${(rMin + 1 until rMax).count(r => emptyRows(r))}, c: ${cMax - cMin} + ${(cMin + 1 until cMax).count(c => emptyRows(c))}")
          d
      }
      .sum
    result
  }
}
