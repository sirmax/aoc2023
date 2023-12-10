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

  /*
  private def countEnclosed(cs: CoordSpace, path: BitSet): Int = {
    def inRow(r: Int) = Iterator
      .from(r * cs.w)
      .take(cs.w)
      .map(path)
      .foldLeft((0, false)) {
        case ((n, false), false) => (n, false)
        case ((n, false), true)  => (n, true)
        case ((n, true), false)  => (n + 1, true)
        case ((n, true), true)   => (n, false)
      }
      ._1

    Iterator.from(0).take(cs.h).map(inRow).sum
  }
   */
  private def enclosed(input: Input, path: BitSet): BitSet = {
    enum VDirection:
      case U, D

    case class State(acc: BitSet, inside: Boolean, separatorStart: Option[VDirection])

    val sAdj = mutuallyAdjacent(input, input.s)
    val sCell =
      List(input.s.u, input.s.r, input.s.d, input.s.l).map(_.exists(c => path(c.i) && sAdj.contains(c))) match {
        case List(true, true, false, false) => Cell.`L`
        case List(true, false, true, false) => Cell.`|`
        case List(true, false, false, true) => Cell.`J`

        case List(false, true, true, false) => Cell.`F`
        case List(false, true, false, true) => Cell.`-`

        case List(false, false, true, true) => Cell.`7`
      }

    val cells = Array.copyOf(input.cells, input.cells.length)
    cells(input.s.i) = sCell

    Iterator
      .from(0)
      .grouped(input.cs.w)
      .take(input.cs.h)
      .map { row =>
        row
          .scanLeft(State(BitSet.empty, false, Option.empty)) { (s, i) =>
            if (path(i)) {
              (s.separatorStart, cells(i)) match {
                case (Some(_), Cell.`-`)            => s
                case (Some(VDirection.U), Cell.`J`) => s.copy(separatorStart = None)
                case (Some(VDirection.U), Cell.`7`) => s.copy(separatorStart = None, inside = !s.inside)
                case (Some(VDirection.D), Cell.`7`) => s.copy(separatorStart = None)
                case (Some(VDirection.D), Cell.`J`) => s.copy(separatorStart = None, inside = !s.inside)
                case (None, Cell.`|`)               => s.copy(inside = !s.inside)
                case (None, Cell.`L`)               => s.copy(separatorStart = Some(VDirection.U))
                case (None, Cell.`F`)               => s.copy(separatorStart = Some(VDirection.D))
              }
            } else if (s.inside && s.separatorStart.isEmpty) s.copy(acc = s.acc + i)
            else s
          }
      }
//      .tapEach(ss => println(s"${ss.last.acc}, $ss"))
      .map(_.last.acc)
      .reduce(_ ++ _)
  }

  private def showPath(cs: CoordSpace, path: BitSet): String = {
    Iterator
      .from(0)
      .map(i => if (path(i)) "#" else ".")
      .grouped(cs.w)
      .take(cs.h)
      .map(_.mkString)
      .mkString("\n")
  }

  def part1(input: Input): String > Effects = {
//    println(s"$input")

    val result = findPath(input).map(_.size / 2).getOrElse(-1)
    s"$result"
  }

  def part2(input: Input): String > Effects = {
    val result = findPath(input)
//      .tapEach(p => println(showPath(input.cs, p)))
      .headOption
      .map(enclosed(input, _))
//      .tapEach(p => println(showPath(input.cs, p)))
      .headOption
      .map(_.size)
      .getOrElse(-1)
    s"$result"
  }
}
