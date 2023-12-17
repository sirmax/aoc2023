import aoc.cartesian.*
import cats.Show
import cats.data.NonEmptyList
import cats.syntax.all.*
import kyo.>
import kyo.apps.App.Effects
import kyo.tries.Tries

import scala.annotation.tailrec
import scala.collection.immutable.{BitSet, Queue, SortedMap, SortedSet}

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

    val result = aStar[Node](
      init = NonEmptyList.of(Node(cStart, Direction.E), Node(cStart, Direction.S)),
      edges = { n => edges(n.d)(n.c.idx) },
      goalReached = { n => n.c == cGoal },
      weightHeuristic = { n => weightHeuristic(n) },
    )
    println(s"nIterations=${result.nIterations} weight=${result.bestWeight}")
    // println(renderMap(input, result.path))
    result.bestWeight
  }

  case class AStarResult[Node](bestWeight: Int, path: List[Node], nIterations: Int)

  private def aStar[Node: Ordering](
    init: NonEmptyList[Node],
    edges: Node => List[(Node, Int)],
    goalReached: Node => Boolean,
    weightHeuristic: Node => Int,
  ): AStarResult[Node] = {
    case class Step(n: Node, moves: List[Node] = Nil, weight: Int = 0) {
      val aStarWeight: Int = weight + weightHeuristic(n)
    }

    object Step {
      given Ordering[Step] = Ordering.by(s => (s.aStarWeight, s.n))
    }

    case class State(
      best: Option[Step] = None,
      // Can be replaced with a Vector if there is a Node => Int mapping, and the full range of Nodes is known.
      weights: Map[Node, Int] = Map.empty[Node, Int],
      nIterations: Int = 0,
    ) {
      def weight(n: Node): Int = weights.getOrElse(n, Int.MaxValue)

      def next(s: Step, isAtGoal: Boolean): State = {
        State(
          best = Option.when(isAtGoal)(s).filter(s => best.forall(_.weight > s.weight)).orElse(best),
          weights = weights.updated(s.n, s.weight),
          nIterations = nIterations + 1,
        )
      }

      def next: State = copy(nIterations = nIterations + 1)
    }

    @tailrec def recur(
      todo: SortedSet[Step] = SortedSet.from(init.toList.map(Step(_))),
      state: State = State(),
    ): State = {
      todo.headOption match {
        case Some(s) =>
          if (s.weight < state.weight(s.n)) {
            val atGoal = goalReached(s.n)
            val state1 = state.next(s, atGoal)

            val newTodo = for {
              (n, addedWeight) <- edges(s.n)
            } yield Step(n, n :: s.moves, addedWeight + s.weight)

            recur(todo.tail ++ newTodo, state1)
          } else recur(todo.tail, state.next)

        case _ => state
      }
    }
    val finalState = recur()
    AStarResult(
      bestWeight = finalState.best.map(_.weight).getOrElse(Int.MaxValue),
      path = finalState.best.map(_.moves).getOrElse(List.empty),
      nIterations = finalState.nIterations,
    )
  }

  private def renderMap(input: Input, moves: List[Node]): String = {
    val movesMask = BitSet.fromSpecific(moves.map(_.c.idx))
    input.cs.render { idx =>
      if (movesMask(idx)) input.at(idx).toString
      else " "
    }
  }
}
