import Day10.Cell
import cats.data.NonEmptyList
import cats.syntax.all.*
import kyo.>
import kyo.App.Effects
import kyo.tries.Tries

import scala.annotation.tailrec
import scala.collection.immutable.{BitSet, Queue}

object Day10 extends util.AocApp(2023, 10) {
  case class Input(cs: CoordSpace, cells: IndexedSeq[Cell]) {
    override def toString: String =
      s"$cs\n${cells.iterator.grouped(cs.w).map(_.mkString).mkString("", "\n", "\n")}"

    def s: Coord = cs.coord(cells.indexOf(Cell.`S`))
  }

  enum Cell {
    case `S`, `.`, `|`, `-`, `L`, `J`, `7`, `F`

    def adjacentDirections: List[HVDirection] = this match
      case Cell.`S` => List(HVDirection.U, HVDirection.D, HVDirection.L, HVDirection.R)
      case Cell.`.` => List.empty
      case Cell.`|` => List(HVDirection.U, HVDirection.D)
      case Cell.`-` => List(HVDirection.L, HVDirection.R)
      case Cell.`L` => List(HVDirection.U, HVDirection.R)
      case Cell.`J` => List(HVDirection.U, HVDirection.L)
      case Cell.`7` => List(HVDirection.D, HVDirection.L)
      case Cell.`F` => List(HVDirection.D, HVDirection.R)
  }

  object Cell {
    def fromOutgoingDirections(dirs: HVDirection*): Option[Cell] = {
      val ds = dirs.toSet
      Cell.values.find(_.adjacentDirections.toSet == ds)
    }
  }

  enum HVDirection {
    case U, D, L, R

    def inverse: HVDirection = this match {
      case HVDirection.U => HVDirection.D
      case HVDirection.D => HVDirection.U
      case HVDirection.L => HVDirection.R
      case HVDirection.R => HVDirection.L
    }
  }

  case class Coord(cs: CoordSpace, idx: Int) {
    def r: Int = idx / cs.w
    def c: Int = idx % cs.w
    def rc: (Int, Int) = (r, c)

    def next(dir: HVDirection): Option[Coord] = dir match {
      case HVDirection.U => Option.when(idx > cs.w)(copy(idx = idx - cs.w))
      case HVDirection.D => Option.when((idx + cs.w) < cs.size)(copy(idx = idx + cs.w))
      case HVDirection.L => Option.when((idx % cs.w) != 0)(copy(idx = idx - 1))
      case HVDirection.R => Option.when(((idx + 1) % cs.w) != 0)(copy(idx = idx + 1))
    }
  }

  case class CoordSpace(w: Int, h: Int) {
    val size: Int = w * h

    def coord(i: Int): Coord = Coord(this, i)
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

  private def adjacent(cell: Cell, c: Coord): List[Coord] = cell.adjacentDirections.flatMap(c.next)

  private def mutuallyAdjacent(input: Input, c: Coord): List[Coord] = {
    adjacent(input.cells(c.idx), c).filter(c1 => adjacent(input.cells(c1.idx), c1).contains(c))
  }

  /*
  private def findPathCoordsCells(input: Input): Option[IndexedSeq[(Coord, Cell)]] = {
    input
      .cells(input.s.idx)
      .adjacentDirections
      .tupleLeft(input.s)
      .collectFirstSome { (c0, dir0) =>
        // Build a path of adjacent nodes. A full loop _ends_ with `c0`.
        // That is to distinct it from an incomplete path.
        val itPath: Iterator[(Coord, Cell)] = Iterator.unfold((c0, dir0).some) {
          case None => None

          case Some((c, dir)) =>
            c.next(dir).flatMap {
              case `c0` =>
                // Loop's closed. Replace `c0` with a compatible 2-sided pipe.
                val cell = Cell.fromOutgoingDirections(dir0, dir.inverse).get
                ((c0, cell), None).some // Loop's closed
              case c1 =>
                val cell = input.cells(c1.idx)
                val state1 = cell.adjacentDirections
                  .filter(_ != dir.inverse)
                  .some
                  .collectFirst { case List(dir1) => (c1, dir1) }
                ((c1, cell), state1).some
            }
        }
        itPath.toIndexedSeq.some.filter(_.lastOption.exists(_._1 == c0))
      }
  }
   */

  private def findPathCoords(input: Input): Option[IndexedSeq[Coord]] = {
    input
      .cells(input.s.idx)
      .adjacentDirections
      .tupleLeft(input.s)
      .collectFirstSome { (c0, dir0) =>
        // Build a path of adjacent nodes. A full loop _ends_ with `c0`.
        // That is to distinct it from an incomplete path.
        val itPath = Iterator.unfold((c0, dir0).some) {
          case None => None
          case Some((c, dir)) =>
            c.next(dir).flatMap {
              case `c0` => (c0, None).some // Loop's closed
              case c1 =>
                input.cells(c1.idx).adjacentDirections.filter(_ != dir.inverse) match {
                  case List(dir1) => (c1, (c1, dir1).some).some
                  case _          => (c1, None).some // Dead end
                }
            }
        }
        itPath.toIndexedSeq.some.filter(_.lastOption.contains(c0))
      }
  }

  private def findPath(input: Input): Option[BitSet] =
    findPathCoords(input).map(_.foldLeft(BitSet.empty)(_ + _.idx))

  private def enclosed(input: Input, path: BitSet): BitSet = {
    enum VDirection:
      case U, D

    case class State(acc: BitSet, inside: Boolean, separatorStart: Option[VDirection])

    val sAdj = mutuallyAdjacent(input, input.s)
    val sCell =
      List(HVDirection.U, HVDirection.R, HVDirection.D, HVDirection.L)
        .map(input.s.next)
        .map(_.exists(c => path(c.idx) && sAdj.contains(c))) match {
        case List(true, true, false, false) => Cell.`L`
        case List(true, false, true, false) => Cell.`|`
        case List(true, false, false, true) => Cell.`J`

        case List(false, true, true, false) => Cell.`F`
        case List(false, true, false, true) => Cell.`-`

        case List(false, false, true, true) => Cell.`7`
      }

    val cells = input.cells.updated(input.s.idx, sCell)

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

    val result = findPathCoords(input).map(_.size / 2).getOrElse(-1)
    s"$result"
  }

  def part2(input: Input): String > Effects = {
    part2_shoelacePicks(input)
  }

  /** Use geometry. Kudos to @jurisk for this. */
  def part2_shoelacePicks(input: Input): String > Effects = {
    val result = findPathCoords(input)
      .map { coords =>
        // https://en.wikipedia.org/wiki/Shoelace_formula
        val area = (coords.iterator ++ Iterator(coords.head))
          .sliding(2)
          .map { case Seq(c1, c2) => (c1.r + c2.r) * (c1.c - c2.c) }
          .sum
          .abs / 2
        // Now, using Pick's theorem get the number of inner points from area and the number of outer points.
        // https://en.wikipedia.org/wiki/Pick%27s_theorem
        val inside = area + 1 - coords.size / 2
        inside
      }
      .getOrElse(-1)

    s"$result"
  }

  def _part2(input: Input): String > Effects = {
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
