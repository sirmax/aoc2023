import aoc.cartesian.*
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
    val result = load(rollUntilStop(input, Direction.N))
    s"$result"
  }

  def part2(input: Input): String > Effects = {
    val (start, loop) = Iterator
      .iterate(input)(spinCycle)
      .scanLeft(IndexedSeq.empty[Input].asLeft[(IndexedSeq[Input], IndexedSeq[Input])]) {
        case (Left(acc), i) =>
          acc.indexOf(i).some.filter(_ >= 0).map(acc.splitAt).toRight(acc :+ i)
        case (right, _) => right
      }
      .collect { case Right(x) => x }
      .next()
    val iNth = (1000000000 - start.size) % loop.size
    // println(s"start=${start.size}, loop=${loop.size}, iNth=$iNth ${loop.map(load).zipWithIndex}")
    val result = load(loop(iNth))
    s"$result"
  }

  def spinCycle(input: Input): Input = {
    List(Direction.N, Direction.W, Direction.S, Direction.E)
      .foldLeft(input)(rollUntilStop)
  }

  def rollUntilStop(input: Input, direction: Direction): Input = {
    val cs = input.cs
    val traversal = direction match {
      case Direction.N => cs.colNums.iterator.flatMap(x => cs.colCoords(x))
      case Direction.S => cs.colNums.iterator.flatMap(x => cs.colCoords(x).toArray.reverseIterator)
      case Direction.W => cs.rowNums.iterator.flatMap(y => cs.rowCoords(y))
      case Direction.E => cs.rowNums.iterator.flatMap(y => cs.rowCoords(y).toArray.reverseIterator)
    }
    val rRocks1 = traversal
      .filter(c0 => input.rRocks(c0.idx))
      .foldLeft(BitSet.empty) { (rRocks, c0) =>
        val c1 = c0.tailRecM[cats.Id, Coord] { c =>
          c.next(direction).filter(c => !input.sRocks(c.idx) && !rRocks(c.idx)).toLeft(c)
        }
        rRocks + c1.idx
      }
    input.copy(rRocks = rRocks1)
  }

  def load(input: Input): Int = {
    input.rRocks.iterator.map { idx =>
      val c = input.cs.coord(idx)
      c.cs.h - c.r
    }.sum
  }
}
