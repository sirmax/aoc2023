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
      val cs = input.cs
      Iterator.range(0, cs.size).grouped(cs.w).map(_.map(input.at(_).show).mkString).mkString("\n")
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

    given Show[Option[Mirror]] = {
      case Some(Mirror.NE) => "/"
      case Some(Mirror.NW) => "\\"
      case Some(Mirror.H)  => "-"
      case Some(Mirror.V)  => "|"
      case None            => "."
    }
  }

  // TODO: Consider extracting from here, Day14, Day13, and Day10
  enum ClockDirection {
    case CW, CCW

    def inverse: ClockDirection = this match {
      case ClockDirection.CW  => ClockDirection.CCW
      case ClockDirection.CCW => ClockDirection.CW
    }
  }

  // TODO: Consider extracting from here, Day14, Day13, and Day10
  enum HVDirection {
    case N, E, S, W

    def turn(d: ClockDirection): HVDirection = {
      // Assuming the NESW order
      val delta = d match
        case ClockDirection.CW  => +1
        case ClockDirection.CCW => -1

      HVDirection.fromOrdinal((this.ordinal + delta) % 4)
    }

    def inverse: HVDirection = {
      // Assuming the NESW order
      HVDirection.fromOrdinal((this.ordinal + 2) % 4)
    }
  }

  // TODO: Consider extracting from here, Day14, Day13, and Day10
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

  // TODO: Consider extracting from here, Day14, Day13, and Day10
  case class CoordSpace(w: Int, h: Int) {
    val size: Int = w * h

    def coord(i: Int): Coord = Coord(this, i)
    def coord(x: Int, y: Int): Coord = Coord(this, w * y + x)

    def rowNums: Range = 0 until h
    def colNums: Range = 0 until w

    def coords: Iterator[Coord] = Iterator.range(0, size).map(coord)
    def rowCoords(r: Int): Iterator[Coord] = colNums.iterator.map(coord(_, r))
    def colCoords(c: Int): Iterator[Coord] = rowNums.iterator.map(coord(c, _))

    def render(empty: String, masks: Seq[(String, BitSet)]): String = {
      def charAt(idx: Int) = masks.collectFirstSome((ch, mask) => Option.when(mask(idx))(ch)).getOrElse(empty)
      Iterator.range(0, size).map(charAt).grouped(w).map(_.mkString).mkString("\n")
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
    // 5151 too low :(
    val result = energized(input).size
    s"$result"
  }

  def part2(input: Input): String > Effects = {
    val result = "???"
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

  private def adjacency(input: Input): Map[HVDirection, Map[Coord, Coord]] = {
    def pairs(d: HVDirection): Map[Coord, Coord] =
      input.cs.coords.flatMap(c => input.at(c).as(c).flatMap(findPair(input, _, d)).tupleLeft(c)).toMap

    val sPairs = pairs(HVDirection.S)
    val ePairs = pairs(HVDirection.E)
    Map(
      HVDirection.N -> sPairs.map(_.swap),
      HVDirection.E -> ePairs,
      HVDirection.S -> sPairs,
      HVDirection.W -> ePairs.map(_.swap),
    )
  }

  private def findPair(input: Input, c: Coord, d: HVDirection): Option[Coord] =
    c.tailRecM(_.next(d).map(c => input.at(c).as(c).toRight(c)))

  // private def energized(input: Input): BitSet = {
  //   val c0 = input.cs.coord(0)
  //   val d0 = HVDirection.E
  //   val c1 = findPair(input, c0, d0)
  //   val adj = adjacency(input)
  //     .updatedWith(d0)(_.map(_.updatedWith(c0)(_.orElse(findPair(input, c0, d0)))))
  //
  //   @tailrec def rec(todo: List[(Coord, HVDirection)], visited: Map[(Coord, HVDirection), Option[Coord]]):
  // }

  private def energized(input: Input): BitSet = {
    @tailrec def rec(todo: List[(Coord, HVDirection)], visited: Map[HVDirection, BitSet]): BitSet = {
      todo match {
        case (c, d) :: rest if !visited(d)(c.idx) =>
          val maybeMirror = input.at(c)
          val next        = maybeMirror.map(applyMirror(_)(d)).getOrElse(List(d)).flatMap(d => c.next(d).tupleRight(d))
          val todo1       = next ++: rest
          val visited1    = visited.updatedWith(d)(_.map(_ + c.idx))
          // println(
          //   s"energized((${todo.head}, _), next=$next\n${input.cs
          //       .render(
          //         ".",
          //         (maybeMirror.map(_.some.show).getOrElse("*"), BitSet(c.idx)) :: (
          //           "@",
          //           BitSet.fromSpecific(todo1.headOption.map(_._1.idx)),
          //         ) :: ("O", BitSet.fromSpecific(todo1.map(_._1.idx))) :: visited1.toList.map((d, mask) => (d.toString.toLowerCase, mask)),
          //       )}",
          // )
          // assert(visited1.values.forall(_.forall(idx => idx >= 0 && idx < input.cs.size)))
          rec(todo1, visited1)
        case _ :: rest => rec(rest, visited)
        case _         => visited.values.reduce(_ ++ _)
      }
    }
    val result = rec(List(input.cs.coord(0) -> HVDirection.E), HVDirection.values.toList.tupleRight(BitSet.empty).toMap)
    // println(input.cs.render(" ", Mirror.values.toList.map(m => (m.some.show, input.mirrors(m))) :+ (".", result)))
    result
  }
}
