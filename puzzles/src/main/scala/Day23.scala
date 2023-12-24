import aoc.cartesian.{Coord, CoordSpace, Direction}
import cats.Show
import cats.data.NonEmptyList
import cats.syntax.all.*
import kyo.>
import kyo.apps.App.Effects
import kyo.tries.Tries

import scala.annotation.tailrec
import scala.collection.immutable.{BitSet, Queue, SortedSet}

object Day23 extends util.AocApp(2023, 23) {
  private given Ordering[Coord] = Ordering.by(_.idx)

  case class Input(
    cs: CoordSpace,
    paths: BitSet = BitSet.empty,
    slopes: Map[Direction, BitSet] = Direction.values.toList.tupleRight(BitSet.empty).toMap,
  ) {
    def addPath(idx: Int): Input = copy(paths = paths + idx)
    def addSlope(d: Direction, idx: Int): Input = copy(slopes = slopes.updatedWith(d)(_.map(_ + idx)))

    def start: Coord = cs.coord(1)
    def goal: Coord = cs.coord(cs.size - 2)
  }

  object Input {
    given Show[Input] = i => {
      val layers = List("S" -> BitSet(i.start.idx), "G" -> BitSet(i.goal.idx), " " -> i.paths) ++
        i.slopes.map((d, mask) => "^>v<" (d.ordinal).show -> mask)
      i.cs.render("#", layers)
    }
  }

  def parseInput(s: String): Input > Effects = {
    import cats.parse.{Numbers as N, Rfc5234 as R, Parser as P}

    val cell: P[(Input, Int) => Input] = P.charIn(".^>v<#").map {
      case '.' => (_: Input).addPath(_)
      case '^' => (_: Input).addSlope(Direction.N, _)
      case '>' => (_: Input).addSlope(Direction.E, _)
      case 'v' => (_: Input).addSlope(Direction.S, _)
      case '<' => (_: Input).addSlope(Direction.W, _)
      case _   => (i: Input, _: Int) => i
    }

    val input = (cell.rep.repSep(R.lf) <* R.lf.?).map { rows =>
      val cs = CoordSpace(w = rows.head.size, h = rows.size)
      rows.flatten.zipWithIndex.foldLeft(Input(cs)) { case (i, (addCell, idx)) => addCell(i, idx) }
    }

    input.parseAll(s).leftMap(e => throw new IllegalArgumentException(s"Failed to parse\n${e.show}")).merge
  }

  def part1(input: Input): String > Effects = {
    // println(input.show)
    val result = solve(input)
    s"$result"
  }

  def part2(input: Input): String > Effects = {
    val result = solve(input.copy(paths = input.paths ++ input.slopes.values.reduce(_ ++ _), slopes = Map.empty))
    s"$result"
  }

  private def solve(input: Input): Int = {
    val paths      = collectPaths(input)
    val pathsBySrc = paths.toList.groupBy(_.src)

    case class Step(at: Coord, visitedNodes: BitSet, length: Int) {
      def next(p: Path): Option[Step] = {
        Option.when(!visitedNodes(p.dst.idx)) {
          Step(p.dst, visitedNodes + p.dst.idx, length = length + p.length)
        }
      }
    }

    object Step {
      def start(at: Coord): Step = Step(at, visitedNodes = BitSet(at.idx), length = 0)
    }

    case class State(n: Int = 0, nCacheHits: Int = 0, best: Option[Int] = None) {
      def next(s: Step): State = {
        val best = Option.when(s.at == input.goal)(s.length).filter(_ > this.best.getOrElse(0)).orElse(this.best)
        copy(n = n + 1, best = best)
      }
    }

    @tailrec def recur(todo: List[Step], state: State): State = todo match {
      case step :: todoRest =>
        val nextSteps = pathsBySrc.getOrElse(step.at, List.empty).flatMap(step.next)
        recur(nextSteps ++: todoRest, state.next(step))

      case Nil => state
    }

    val finalState = recur(List(Step.start(input.start)), State())
    finalState.best.getOrElse(0)
  }

  private def collectPaths(input: Input): IndexedSeq[Path] = {
    case class PathAcc(c0: Coord, d0: Direction, c: Coord, d: Direction, twoWay: Boolean, length: Int) {
      def next(c: Coord, d: Direction, twoWay: Boolean): PathAcc =
        copy(c = c, d = d, twoWay = this.twoWay && twoWay, length = length + 1)

      def inverse: Option[PathAcc] = Option.when(twoWay)(copy(c0 = c, d0 = d.inverse, c = c0, d = d0.inverse))
    }

    object PathAcc {
      def start(c0: Coord, d0: Direction): PathAcc = PathAcc(c0, d0, c0, d0, twoWay = true, length = 0)
    }

    @tailrec def recur(todo: List[PathAcc], paths: Map[(Coord, Direction), PathAcc]): List[Path] = {
      todo match {
        case p :: todoRest if p.c == input.goal =>
          recur(todoRest, paths + ((p.c0, p.d0) -> p))

        case p :: todoRest =>
          val next = for {
            d <- Direction.values.toList
            if d != p.d.inverse
            c     <- p.c.next(d)
            isPath = input.paths(c.idx)
            if isPath || input.slopes.get(d).exists(_(c.idx))
          } yield p.next(c, d, twoWay = isPath)

          next match {
            case Nil      => recur(todoRest, paths)
            case p :: Nil => recur(p :: todoRest, paths)
            case forked =>
              val paths1 = paths ++ (p :: p.inverse.toList).map(p => (p.c0, p.d0) -> p)
              val p0     = p
              val addTodo = forked
                .map(p => PathAcc.start(p0.c, p.d).next(p.c, p.d, p.twoWay))
                .filter(p => !paths1.keySet((p.c0, p.d0)))
              recur(addTodo ++: todoRest, paths1)
          }

        case Nil =>
          paths.values.toList.zipWithIndex.map((p, id) => Path(id, p.c0, p.c, p.length)).sortBy(p => (p.src, p.dst))
      }
    }

    val result = recur(List(PathAcc.start(input.start, Direction.S)), Map.empty)
    result.toIndexedSeq
  }

  private case class Path(id: Int, src: Coord, dst: Coord, length: Int)
}
