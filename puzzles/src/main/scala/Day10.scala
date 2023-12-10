import Day10.Cell
import cats.data.NonEmptyList
import cats.syntax.all.*
import kyo.>
import kyo.App.Effects
import kyo.tries.Tries

import scala.annotation.tailrec
import scala.collection.immutable.{BitSet, Queue}

object Day10 extends util.AocApp(2023, 10) {
  case class Input(cs: CoordSpace, cells: Array[Cell]) {
    override def toString: String =
      s"$cs\n${cells.iterator.grouped(cs.w).map(_.mkString).mkString("", "\n", "\n")}"

    def s: cs.Coord = cs.coord(cells.indexOf(Cell.`S`))
  }

  enum Cell:
    case `S`, `.`, `|`, `-`, `L`, `J`, `7`, `F`

  case class CoordSpace(w: Int, h: Int) {
    private val size = w * h

    opaque type Coord = Int

    def coord(i: Int): Coord = i

    extension (c: Coord) {
      def i: Int = c
      def rc: (Int, Int) = (c / w, c % w)

      def u: Option[Coord] = Option.when(c >= w)(c - w)
      def d: Option[Coord] = Option.when((c + w) < size)(c + w)
      def l: Option[Coord] = Option.when((c % w) != 0)(c - 1)
      def r: Option[Coord] = Option.when(((c + 1) % w) != 0)(c + 1)
    }
  }

  def parseInput(s: String): Input > Effects = {
    import cats.parse.{Numbers as N, Rfc5234 as R, Parser as P}
    import cats.syntax.all.*

    val sps    = R.sp.rep.void
    val slong  = N.signedIntString.map(_.toLong).surroundedBy(R.sp.rep0)
    val slongs = slong.rep
    val nl     = P.char('\n')

    val cell = P.charIn(Cell.values.map(_.toString.head)).string.map(Cell.valueOf)
    val row  = cell.rep.map(_.toList.toArray)

    val input =
      (row.repSep(nl) <* nl.?).map(rows => Input(CoordSpace(rows.head.length, rows.length), rows.toList.reduce(_ ++ _)))

    val result = input.parseAll(s)
    result
      .leftMap(e => s"$e ${e.input.map(s => s.slice(e.failedAtOffset, e.failedAtOffset + 32))}")
      .fold(Tries.fail, identity)
  }

  private def adjacent(cell: Cell, cs: CoordSpace, c: cs.Coord): List[cs.Coord] = cell match {
    case Cell.`S` => List(c.u, c.d, c.l, c.r).flatten
    case Cell.`.` => List.empty
    case Cell.`|` => List(c.u, c.d).flatten
    case Cell.`-` => List(c.l, c.r).flatten
    case Cell.`L` => List(c.u, c.r).flatten
    case Cell.`J` => List(c.u, c.l).flatten
    case Cell.`7` => List(c.d, c.l).flatten
    case Cell.`F` => List(c.d, c.r).flatten
  }

  private def mutuallyAdjacent(input: Input, c: input.cs.Coord): List[input.cs.Coord] = {
    adjacent(input.cells(c.i), input.cs, c).filter(c1 => adjacent(input.cells(c1.i), input.cs, c1).contains(c))
  }

  private def findPath(input: Input): Option[BitSet] = {
    @tailrec def rec(c0: input.cs.Coord, c: input.cs.Coord, path: BitSet): Option[BitSet] = {
      val atCell             = input.cells(c.i)
      val (visited, toVisit) = mutuallyAdjacent(input, c).partition(c => path(c.i))
      /*
      println {
        val sPath = Iterator
          .from(0)
          .map(i =>
            if (i == c.i) input.cells(c.i).toString
            else if (path(i)) "#"
            else input.cs.coord(i).some.filter(toVisit.contains).as(input.cells(i).toString).getOrElse(" "),
          )
          .grouped(input.cs.w)
          .take(input.cs.h)
          .map(_.mkString)
          .mkString("\n")
        val hSep = "-" * input.cs.w
        val header = s"c0=$c0, c=$c ${ c.rc }, visited=$visited, toVisit=$toVisit, path=${ path.toList.sorted }"
        s"\n$header\n$hSep$sPath\n$hSep"
      }
       */
      if (visited.exists(_ != c0))
        (path + c.i).some
      else {
        toVisit.headOption match {
          case Some(c1) => rec(c, c1, path + c.i)
          case None     => None
        }
      }
    }

    val path0 = BitSet.empty + input.s.i
    mutuallyAdjacent(input, input.s).collectFirstSome(rec(input.s, _, path0))
  }

  def part1(input: Input): String > Effects = {
//    println(s"$input")

    val result = findPath(input).map(_.size / 2).getOrElse(-1)
    s"$result"
  }

  def part2(input: Input): String > Effects = {
    val result = "???"
    s"$result"
  }
}
