import Day10.Cell
import cats.data.NonEmptyList
import cats.syntax.all.*
import kyo.>
import kyo.App.Effects
import kyo.tries.Tries

import scala.annotation.tailrec
import scala.collection.immutable.Queue

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

  def part1(input: Input): String > Effects = {
//    println(s"$input")

    @tailrec def rec(next: Queue[input.cs.Coord], distances: Array[Int]): Int = {
//      println()
//      println(s"$next\n${distances.iterator.grouped(input.cs.w).map(_.mkString).mkString("\n")}")
      val (c +: rest) = next
      val atCell      = input.cells(c.i)
      val atDistance  = distances(c.i)
      val mutuallyAdjacent =
        adjacent(atCell, input.cs, c).filter(c1 => adjacent(input.cells(c1.i), input.cs, c1).contains(c))
      val (toVisit, visited) = mutuallyAdjacent.partition(c => distances(c.i) == 0)
//      println(s"toVisit=$toVisit, visited=$visited")

      if (visited.exists(c => distances(c.i) == atDistance + 1))
        atDistance
      else {
        toVisit.foreach(c => distances(c.i) = atDistance + 1) // Side-effect! GASP!!!
        rec(rest :++ toVisit, distances)
      }
    }

    val distances = Array.fill(input.cells.size)(0)
    distances(input.s.i) = 1
    val result = rec(Queue(input.s), distances)
    s"$result"
  }

  def part2(input: Input): String > Effects = {
    val result = "???"
    s"$result"
  }
}
