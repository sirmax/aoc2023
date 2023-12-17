import aoc.cartesian.*
import aoc.graph.AStar
import cats.Show
import cats.data.NonEmptyList
import cats.syntax.all.*
import kyo.>
import kyo.apps.App.Effects
import kyo.tries.Tries

import scala.annotation.tailrec
import scala.collection.immutable.BitSet

object Day17 extends util.AocApp(2023, 17) {
  case class Input(cs: CoordSpace, map: IndexedSeq[Int]) {
    def at(c: Coord): Int = at(c.idx)
    def at(idx: Int): Int = map(idx)
  }

  object Input {
    given Show[Input] = input => input.cs.render(idx => input.at(idx).toString, Seq.empty)
  }

  def parseInput(s: String): Input > Effects = {
    import cats.parse.{Numbers as N, Rfc5234 as R, Parser as P}

    val int = R.digit.string.map(_.toInt)

    val input = (int.rep.repSep(R.lf) <* R.lf.?).map { rows =>
      val cs = CoordSpace(w = rows.head.length, h = rows.length)
      val map = rows.iterator
        .flatMap(_.iterator)
        .toArray
        .toIndexedSeq
      Input(cs, map)
    }

    input
      .parseAll(s)
      .leftMap(e => s"$e ${e.input.map(s => s.slice(e.failedAtOffset, e.failedAtOffset + 32))}")
      .fold(Tries.fail, identity)
  }

  def part1(input: Input): String > Effects = {
    // println(input.show)
    val result = leastLoss(input, input.cs.coord(0), input.cs.coord(input.cs.size - 1), 1, 3)
    s"$result"
  }

  def part2(input: Input): String > Effects = {
    val result = leastLoss(input, input.cs.coord(0), input.cs.coord(input.cs.size - 1), 4, 10)
    s"$result"
  }

  case class Node(c: Coord, d: Direction)

  object Node {
    given Ordering[Node] = Ordering.by(n => (n.c.idx, n.d.ordinal))
  }

  private def leastLoss(input: Input, cStart: Coord, cGoal: Coord, minStreak: Int, maxStreak: Int): Int = {
    // Outgoing c.idx == n in inner IndexedSeq(n)
    val edges: Map[Direction, IndexedSeq[List[(Node, Int)]]] = {
      Direction.values.toList.fproduct { d0 =>
        input.cs.indexes.map { idx =>
          val c0 = input.cs.coord(idx)
          Rotation.values.toList.map(d0.turn).flatMap { d1 =>
            List
              .unfold(c0)(c => c.next(d1).map(c => (c, c)))
              .scanLeft((Node(c0, d1), 0)) { case ((_, loss), c) => (Node(c, d1), loss + input.at(c)) }
              .drop(1) // drop the initial 0
              .slice(minStreak - 1, maxStreak)
          }
        }
      }.toMap
    }

    def weightHeuristic(n: Node): Int = {
      // Fairly arbitrary, TBH
      val distance = n.c.manhattanDistance(cGoal)
      val directionPenalty = n.d match {
        case Direction.N => 10
        case Direction.E => 0
        case Direction.S => 0
        case Direction.W => 10
      }
      distance + directionPenalty
    }

    val result = AStar.runIndexed[Node](
      init = NonEmptyList.of(Node(cStart, Direction.E), Node(cStart, Direction.S)),
      edges = { n => edges(n.d)(n.c.idx) },
      goalReached = { n => n.c == cGoal },
      weightHeuristic = { n => weightHeuristic(n) },
      nodeIndex = { n => n.c.idx + (n.d.ordinal * n.c.cs.size) },
    )
    println(s"nIterations=${result.nIterations} weight=${result.bestWeight}")
    // println(renderMap(input, result.path))
    result.bestWeight.getOrElse(sys.error("No result :("))
  }

  private def renderMap(input: Input, moves: List[Node]): String = {
    val movesMask = BitSet.fromSpecific(moves.map(_.c.idx))
    input.cs.render { idx =>
      if (movesMask(idx)) input.at(idx).toString
      else " "
    }
  }
}
