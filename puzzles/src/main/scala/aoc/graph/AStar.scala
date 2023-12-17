package aoc.graph

import cats.data.NonEmptyList

import scala.annotation.tailrec
import scala.collection.immutable.SortedSet

/** [[https://en.wikipedia.org/wiki/A*_search_algorithm]] */
object AStar {
  case class AStarResult[Node](bestWeight: Option[Int], path: List[Node], nIterations: Int)

  /** An anecdotally faster alternative to [[AStar.run]] that uses `Int`-based index internally. */
  def runIndexed[Node](
    init: NonEmptyList[Node],
    edges: Node => List[(Node, Int)],
    goalReached: Node => Boolean,
    weightHeuristic: Node => Int,
    nodeIndex: Node => Int,
  ): AStarResult[Node] = {
    _run(init, edges, goalReached, weightHeuristic, nodeIndex = nodeIndex, VectorWeights.empty)
  }

  def run[Node: Ordering](
    init: NonEmptyList[Node],
    edges: Node => List[(Node, Int)],
    goalReached: Node => Boolean,
    weightHeuristic: Node => Int,
  ): AStarResult[Node] = {
    _run(init, edges, goalReached, weightHeuristic, nodeIndex = identity, MapWeights.empty)
  }

  private def _run[Node, Idx: Ordering](
    init: NonEmptyList[Node],
    edges: Node => List[(Node, Int)],
    goalReached: Node => Boolean,
    weightHeuristic: Node => Int,
    nodeIndex: Node => Idx,
    emptyWeights: Weights[Idx],
  ): AStarResult[Node] = {
    case class Step(n: Node, idx: Idx, moves: List[Node] = Nil, weight: Int = 0) {
      val aStarWeight: Int = weight + weightHeuristic(n)
    }

    object Step {
      given Ordering[Step] = Ordering.by(s => (s.aStarWeight, s.idx))
    }

    case class State(best: Option[Step] = None, weights: Weights[Idx] = emptyWeights, nIterations: Int = 0) {
      def weight(idx: Idx): Int = weights.at(idx)

      def next(s: Step, isAtGoal: Boolean): State = {
        State(
          best = Option.when(isAtGoal)(s).filter(s => best.forall(_.weight > s.weight)).orElse(best),
          weights = weights.updated(s.idx, s.weight),
          nIterations = nIterations + 1,
        )
      }

      def next: State = copy(nIterations = nIterations + 1)
    }

    @tailrec def recur(
      todo: SortedSet[Step] = SortedSet.from(init.toList.map(n => Step(n, nodeIndex(n)))),
      state: State = State(),
    ): State = {
      todo.headOption match {
        case Some(s) =>
          if (s.weight < state.weight(s.idx)) {
            val atGoal = goalReached(s.n)
            val state1 = state.next(s, atGoal)

            val newTodo = for {
              (n, addedWeight) <- edges(s.n)
            } yield Step(n, nodeIndex(n), n :: s.moves, addedWeight + s.weight)

            recur(todo.tail ++ newTodo, state1)
          } else recur(todo.tail, state.next)

        case _ => state
      }
    }
    val finalState = recur()
    AStarResult(
      bestWeight = finalState.best.map(_.weight),
      path = finalState.best.map(_.moves).getOrElse(List.empty),
      nIterations = finalState.nIterations,
    )
  }

  private trait Weights[Idx] {
    def at(idx: Idx): Int
    def updated(idx: Idx, weight: Int): Weights[Idx]
  }

  private case class MapWeights[Idx](data: Map[Idx, Int]) extends Weights[Idx] {
    def at(idx: Idx): Int = data.getOrElse(idx, Int.MaxValue)
    def updated(idx: Idx, weight: Int): Weights[Idx] = copy(data.updated(idx, weight))
  }

  private object MapWeights {
    def empty[Idx]: MapWeights[Idx] = MapWeights(Map.empty)
  }

  private case class VectorWeights[Idx <: Int](data: Vector[Int]) extends Weights[Idx] {
    def at(idx: Idx): Int = if (idx < data.size && idx >= 0) data(idx) else Int.MaxValue
    def updated(idx: Idx, weight: Int): Weights[Idx] = copy(data.padTo(idx + 1, Int.MaxValue).updated(idx, weight))
  }

  private object VectorWeights {
    def empty[Idx <: Int]: VectorWeights[Idx] = VectorWeights(Vector.empty)
  }
}
