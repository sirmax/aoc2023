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
    case class State(
      c: Coord,
      d: Direction,
      streak: Int,
      moves: List[Coord] = Nil,
      heatLoss: Int = 0,
    )

    case class Acc(
      best: Option[State] = None,
      minHeatLoss: Map[(Direction, Int), Vector[Int]] = {
        (Direction.values.toList, (1 to maxStreak).toList).tupled
          .tupleRight(Vector.fill(input.cs.size)(Int.MaxValue))
          .toMap
      },
      nIterations: Long = 0,
    ) {
      def lossAt(c: Coord, d: Direction, streak: Int): Int = minHeatLoss((d, streak))(c.idx)

      def add(c: Coord, d: Direction, streak: Int, heatLoss: Int, newBest: Option[State] = None): Acc = {
        Acc(
          best = newBest.filter(s => best.forall(_.heatLoss > s.heatLoss)).orElse(best),
          minHeatLoss = minHeatLoss.updatedWith((d, streak))(_.map(_.updated(c.idx, heatLoss))),
          nIterations = nIterations + 1,
        )
      }

      def next: Acc = copy(nIterations = nIterations + 1)
    }

    @tailrec def recur(
      todo: Queue[State] = Queue(
        State(cStart, Direction.E, streak = 0),
        // State(cStart, Direction.S, streak = 0),
      ),
      acc: Acc = Acc(),
    ): Option[State] = {
      // val traceEvery = 1L
      val traceEvery = 10_000_000L
      if (acc.nIterations % traceEvery == 0) {
        val map = renderMap(
          input,
          todo.headOption.map(_.moves).getOrElse(List.empty),
          acc.best.map(_.moves).getOrElse(List.empty),
        )
        println(s"${acc.nIterations} best=${acc.best.map(_.heatLoss)} current=${todo.headOption.map(_.heatLoss)}\n$map")
      }

      todo match {
        case s +: todoRest =>
          if (s.streak == 0 || s.heatLoss < acc.lossAt(s.c, s.d, s.streak)) {
            val atGoal = s.c == cGoal
            val acc1   = acc.add(s.c, s.d, s.streak, s.heatLoss, if (atGoal) s.some else acc.best)

            val newTodo = for {
              _ <- List(()) if !atGoal
              (d, streak) <- {
                def turns = List(Rotation.CW, Rotation.CCW).map(r => (s.d.turn(r), 1))
                if (s.streak < minStreak) (s.d, s.streak + 1) :: Nil
                else if (s.streak < maxStreak) (s.d, s.streak + 1) :: turns
                else turns
              }
              c <- s.c.next(d)
              // Don't follow streaks that would go out of bounds
              if Iterator.iterate(c.some)(_.flatMap(_.next(d))).drop(minStreak - streak).next().isDefined
              addedLoss = input.at(c)
            } yield State(c, d, streak, c :: s.moves, addedLoss + s.heatLoss)

            recur(todoRest :++ newTodo, acc1)
          } else recur(todoRest, acc.next)

        case _ => acc.best
      }
    }

    val best = recur().getOrElse(sys.error("Oh no!"))
    println(s"""heatLoss=${best.heatLoss}\n${renderMap(input, best.moves, List.empty)}""")
    best.heatLoss
  }

  private def renderMap(input: Input, moves: List[Coord], best: List[Coord]): String = {
    val movesMask = BitSet.fromSpecific(moves.map(_.idx))
    val bestMask  = BitSet.fromSpecific(best.map(_.idx))
    input.cs.render { idx =>
      if (movesMask(idx)) input.at(idx).toString
      else if (bestMask(idx)) "."
      else " "
    }
  }
}
