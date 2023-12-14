import cats.Show
import cats.data.NonEmptyList
import cats.syntax.all.*
import kyo.>
import kyo.apps.App.Effects
import kyo.tries.Tries

import scala.collection.immutable.BitSet

object Day14 extends util.AocApp(2023, 14) {
  case class Input(cs: CoordSpace, rRocks: BitSet, sRocks: BitSet)

  object Input:
    given Show[Input] = input =>
      Iterator
        .tabulate(input.cs.h)(r =>
          Iterator
            .tabulate(input.cs.w) { c =>
              val idx = input.cs.coord(x = c, y = r).idx
              (input.rRocks(idx), input.sRocks(idx)) match {
                case (false, false) => '.'
                case (false, true)  => '#'
                case (true, false)  => 'O'
                case (true, true)   => 'X'
              }
            }
            .mkString,
        )
        .mkString("\n")

  // TODO: Consider extracting from here, Day13, and Day10
  enum HVDirection {
    case N, S, W, E

    def inverse: HVDirection = this match {
      case HVDirection.N => HVDirection.S
      case HVDirection.S => HVDirection.N
      case HVDirection.W => HVDirection.E
      case HVDirection.E => HVDirection.W
    }
  }

  // TODO: Consider extracting from here, Day13, and Day10
  case class Coord(cs: CoordSpace, idx: Int) {
    def r: Int = idx / cs.w
    def c: Int = idx % cs.w
    def rc: (Int, Int) = (r, c)

    def next(dir: HVDirection): Option[Coord] = dir match {
      case HVDirection.N => Option.when(idx >= cs.w)(copy(idx = idx - cs.w))
      case HVDirection.S => Option.when((idx + cs.w) < cs.size)(copy(idx = idx + cs.w))
      case HVDirection.W => Option.when((idx % cs.w) != 0)(copy(idx = idx - 1))
      case HVDirection.E => Option.when(((idx + 1) % cs.w) != 0)(copy(idx = idx + 1))
    }
  }

  // TODO: Consider extracting from here, Day13, and Day10
  case class CoordSpace(w: Int, h: Int) {
    val size: Int = w * h

    def coord(i: Int): Coord = Coord(this, i)
    def coord(x: Int, y: Int): Coord = Coord(this, w * y + x)

    def rowNums: Range = 0 until h
    def colNums: Range = 0 until w

    def rowCoords(r: Int): Iterator[Coord] = colNums.iterator.map(coord(_, r))
    def colCoords(c: Int): Iterator[Coord] = rowNums.iterator.map(coord(c, _))
  }

  def parseInput(s: String): Input > Effects = {
    import cats.parse.{Numbers as N, Rfc5234 as R, Parser as P}
    import cats.syntax.all.*

    val nl = P.char('\n')

    val map = P
      .charIn(".O#")
      .rep
      .repSep(nl)
      .map { rows =>
        val h = rows.size
        val w = rows.head.size
        val (rRocks, sRocks) = rows.flatten.zipWithIndex.foldl((BitSet.empty, BitSet.empty)) {
          case ((rRocks, sRocks), ('O', i)) => (rRocks + i, sRocks)
          case ((rRocks, sRocks), ('#', i)) => (rRocks, sRocks + i)
          case ((rRcks, sRocks), _)         => (rRcks, sRocks)
        }
        Input(CoordSpace(w, h), rRocks, sRocks)
      }

    val input = map <* nl.?

    val result = input.parseAll(s)

    result
      .leftMap(e => s"$e ${e.input.map(s => s.slice(e.failedAtOffset, e.failedAtOffset + 32))}")
      .fold(Tries.fail, identity)
  }

  def part1(input: Input): String > Effects = {
    // println(input.show)
    val result = load(rollUntilStop(input, HVDirection.N))
    s"$result"
  }

  def part2(input: Input): String > Effects = {
    val result = "???"
    s"$result"
  }

  def rollUntilStop(input: Input, direction: HVDirection): Input = {
    List
      .unfold(input)(i0 => roll1(i0, direction).map(i => (i, i)))
      .lastOption
      .getOrElse(input)
  }

  def roll1(input: Input, direction: HVDirection): Option[Input] = {
    val cs = input.cs
    val traversal = direction match {
      case HVDirection.N =>
        cs.colNums.iterator.flatMap(x => cs.colCoords(x))

      case x => throw MatchError(x)
    }
    val rRocks1 = traversal
      .filter(c0 => input.rRocks(c0.idx))
      .foldLeft(BitSet.empty) { (rRocks, c0) =>
        val c1 = c0
          .next(direction)
          .filter(c => !input.sRocks(c.idx) && !rRocks(c.idx))
          .getOrElse(c0)
        rRocks + c1.idx
      }
    input.copy(rRocks = rRocks1).some.filter(_ != input)
  }

  def load(input: Input): Int = {
    input.rRocks.iterator.map { idx =>
      val c = input.cs.coord(idx)
      c.cs.h - c.r
    }.sum
  }
}
