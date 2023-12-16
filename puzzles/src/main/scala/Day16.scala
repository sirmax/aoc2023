import aoc.cartesian.*
import cats.Show
import cats.data.NonEmptyList
import cats.syntax.all.*
import kyo.>
import kyo.apps.App.Effects
import kyo.tries.Tries

import scala.annotation.tailrec
import scala.collection.immutable.{BitSet, Queue}

object Day16 extends util.AocApp(2023, 16) {
  case class Input(cs: CoordSpace, mirrors: Map[Mirror, BitSet] = Mirror.values.map(_ -> BitSet.empty).toMap) {
    def at(c: Coord): Option[Mirror] = at(c.idx)

    def at(idx: Int): Option[Mirror] = Mirror.values.find(m => mirrors.get(m).exists(_(idx)))

    def updated(c: Coord, m: Mirror): Input = updated(c.idx, m)

    def updated(idx: Int, m: Mirror): Input = {
      val mirrors1 = mirrors.map {
        case (`m`, mask) => m -> (mask + idx)
        case (m, mask)   => m -> (mask - idx)
      }
      copy(mirrors = mirrors1)
    }
  }

  object Input {
    given Show[Input] = { input =>
      input.cs.render(".", Mirror.values.map(m => (m.show, input.mirrors(m))))
    }
  }

  enum Mirror:
    case NE, NW, H, V

  object Mirror {
    def fromChar(c: Char): Option[Mirror] = c.some.collect {
      case '/'  => Mirror.NE
      case '\\' => Mirror.NW
      case '-'  => Mirror.H
      case '|'  => Mirror.V
    }

    given Show[Mirror] = {
      case Mirror.NE => "/"
      case Mirror.NW => "\\"
      case Mirror.H  => "-"
      case Mirror.V  => "|"
    }
  }

  def parseInput(s: String): Input > Effects = {
    import cats.parse.{Numbers as N, Rfc5234 as R, Parser as P}

    val mirror = P.charIn(".\\/-|").map(Mirror.fromChar)

    val input = (mirror.rep.repSep(R.lf) <* R.lf.?).map { rows =>
      val cs = CoordSpace(w = rows.head.length, h = rows.length)
      rows.iterator
        .flatMap(_.iterator)
        .zipWithIndex
        .foldLeft(Input(cs)) {
          case (input, (mirror, idx)) =>
            mirror.foldLeft(input)(_.updated(idx, _))
        }
    }

    input
      .parseAll(s)
      .leftMap(e => s"$e ${e.input.map(s => s.slice(e.failedAtOffset, e.failedAtOffset + 32))}")
      .fold(Tries.fail, identity)
  }

  def part1(input: Input): String > Effects = {
    // println(input.show)
    val energized = runBeam(input)
    // println(input.cs.render(" ", Mirror.values.toList.map(m => (m.show, input.mirrors(m))) :+ (".", energized)))
    val result = energized.size
    s"$result"
  }

  def part2(input: Input): String > Effects = {
    val best = Iterator(
      input.cs.rowCoords(0).map(_ -> HVDirection.S),
      input.cs.rowCoords(input.cs.h - 1).map(_ -> HVDirection.N),
      input.cs.colCoords(0).map(_ -> HVDirection.E),
      input.cs.colCoords(input.cs.w - 1).map(_ -> HVDirection.W),
    ).flatten
      .map((c, d) => runBeam(input, c, d))
      .maxBy(_.size)
    val result = best.size
    s"$result"
  }

  private val applyMirror: Map[Mirror, Map[HVDirection, List[HVDirection]]] = {
    import HVDirection.*
    Map(
      Mirror.NE -> Map((N, List(E)), (S, List(W)), (E, List(N)), (W, List(S))),
      Mirror.NW -> Map((N, List(W)), (S, List(E)), (E, List(S)), (W, List(N))),
      Mirror.H -> Map((N, List(E, W)), (S, List(E, W)), (E, List(E)), (W, List(W))),
      Mirror.V -> Map((N, List(N)), (S, List(S)), (E, List(N, S)), (W, List(N, S))),
    )
  }

  private def runBeam(input: Input): BitSet = runBeam(input, input.cs.coord(0), HVDirection.E)

  private def runBeam(input: Input, c0: Coord, d0: HVDirection): BitSet = {
    @tailrec def rec(todo: List[(Coord, HVDirection)], visited: Map[HVDirection, BitSet]): BitSet = {
      todo match {
        case (c, d) :: rest if !visited(d)(c.idx) =>
          val maybeMirror = input.at(c)
          val next        = maybeMirror.map(applyMirror(_)(d)).getOrElse(List(d)).flatMap(d => c.next(d).tupleRight(d))
          val todo1       = next ++: rest
          val visited1    = visited.updatedWith(d)(_.map(_ + c.idx))
          // println(
          //   s"runBeam((${todo.head}, _), next=$next\n${input.cs
          //       .render(
          //         ".",
          //         (maybeMirror.map(_.show).getOrElse("*"), BitSet(c.idx)) :: (
          //           "@",
          //           BitSet.fromSpecific(todo1.headOption.map(_._1.idx)),
          //         ) :: ("O", BitSet.fromSpecific(todo1.map(_._1.idx))) :: visited1.toList.map((d, mask) => (d.toString.toLowerCase, mask)),
          //       )}",
          // )
          rec(todo1, visited1)
        case _ :: rest => rec(rest, visited)
        case _         => visited.values.reduce(_ ++ _)
      }
    }
    rec(List((c0, d0)), HVDirection.values.toList.tupleRight(BitSet.empty).toMap)
  }
}
