import aoc.cartesian.*
import cats.Show
import cats.data.NonEmptyList
import cats.syntax.all.*
import kyo.>
import kyo.apps.App.Effects
import kyo.tries.Tries

import scala.annotation.tailrec
import scala.collection.immutable.{BitSet, Queue}

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
    println(input.show)
    val result = leastLoss(input, input.cs.coord(0), input.cs.coord(input.cs.size - 1), 1, 3)
    s"$result"
  }

  def part2(input: Input): String > Effects = {
    val result = leastLoss(input, input.cs.coord(0), input.cs.coord(input.cs.size - 1), 4, 10)
    s"$result"
  }

  private def leastLoss(input: Input, cStart: Coord, cGoal: Coord, minStreak: Int, maxStreak: Int): Int = {
    val edges: Map[Direction, IndexedSeq[List[(Coord, Int)]]] = {
      Direction.values.toList.fproduct { d =>
        input.cs.indexes.map { idx =>
          val c0 = input.cs.coord(idx)
          List
            .unfold(c0)(c => c.next(d).map(c => (c, c)))
            .scanLeft((c0, 0)) { case ((_, loss), c) => (c, loss + input.at(c)) }
            .drop(1) // drop the initial 0
            .slice(minStreak - 1, maxStreak)
        }
      }.toMap
    }

    case class State(c: Coord = cStart, d: Option[Direction] = None, moves: List[Coord] = Nil, heatLoss: Int = 0)

    case class Acc(
      best: Option[State] = None,
      minHeatLoss: Map[Direction, Vector[Int]] =
        Direction.values.toList.tupleRight(Vector.fill(input.cs.size)(Int.MaxValue)).toMap,
      nIterations: Int = 0,
    ) {
      def lossAt(c: Coord, d: Direction): Int = minHeatLoss(d)(c.idx)

      def next(s: State, isAtGoal: Boolean): Acc = {
        Acc(
          best = Option.when(isAtGoal)(s).filter(s => best.forall(_.heatLoss > s.heatLoss)).orElse(best),
          minHeatLoss =
            s.d.map(d => minHeatLoss.updatedWith(d)(_.map(_.updated(s.c.idx, s.heatLoss)))).getOrElse(minHeatLoss),
          nIterations = nIterations + 1,
        )
      }

      def next: Acc = copy(nIterations = nIterations + 1)
    }

    @tailrec def recur(todo: Queue[State] = Queue(State()), acc: Acc = Acc()): Acc = {
      todo.match {
        case s +: todoRest =>
          // val traceEvery = 1L
          val traceEvery = 10_000_000L
          // if (acc.nIterations % traceEvery == 0) {
          //   val map = renderMap(
          //     input,
          //     todo.headOption.map(_.moves).getOrElse(List.empty),
          //   )
          //   println(
          //     s"${acc.nIterations} best=${acc.best.map(_.heatLoss)} current=${todo.headOption.map(_.heatLoss)}\n$map",
          //   )
          // }

          if (s.d.forall(d => s.heatLoss < acc.lossAt(s.c, d))) {
            val atGoal = s.c == cGoal
            val acc1   = acc.next(s, atGoal)

            val newTodo = for {
              d              <- s.d.map(d => Rotation.values.map(d.turn)).getOrElse(Direction.values).toList
              (c, addedLoss) <- edges(d)(s.c.idx)
            } yield State(c, d.some, c :: s.moves, addedLoss + s.heatLoss)

            recur(todoRest :++ newTodo, acc1)
          } else recur(todoRest, acc.next)

        case _ => acc
      }
    }

    val acc  = recur()
    val best = acc.best.getOrElse(sys.error("No solution found :("))
    println(s"""nIterations=${acc.nIterations} heatLoss=${best.heatLoss}\n${renderMap(input, best.moves)}""")
    best.heatLoss
  }

  private def renderMap(input: Input, moves: List[Coord]): String = {
    val movesMask = BitSet.fromSpecific(moves.map(_.idx))
    input.cs.render { idx =>
      if (movesMask(idx)) input.at(idx).toString
      else " "
    }
  }
}
