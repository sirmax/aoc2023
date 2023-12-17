package aoc.graph

import cats.data.NonEmptyList

import scala.annotation.tailrec
import scala.collection.immutable.SortedSet

/** [[https://en.wikipedia.org/wiki/A*_search_algorithm]] */
object AStar {
  case class AStarResult[Node](bestWeight: Option[Int], path: List[Node], nIterations: Int)

  def run[Node: Ordering](
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
      bestWeight = finalState.best.map(_.weight),
      path = finalState.best.map(_.moves).getOrElse(List.empty),
      nIterations = finalState.nIterations,
    )
  }
}
