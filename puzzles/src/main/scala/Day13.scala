import cats.Show
import cats.data.NonEmptyList
import cats.syntax.all.*
import kyo.>
import kyo.apps.App.Effects
import kyo.tries.Tries

import scala.collection.immutable.BitSet

object Day13 extends util.AocApp(2023, 13) {
  val (solid, blank) = ('#', '.')
  // val (solid, blank) = ('█', '░')

  case class Input(patterns: NonEmptyList[Pattern])

  object Input:
    given Show[Input] = _.patterns.mkString_("\n\n")

  case class Pattern(cs: CoordSpace, rocks: BitSet)

  object Pattern:
    given Show[Pattern] = p =>
      Iterator
        .tabulate(p.cs.h)(r =>
          Iterator
            .tabulate(p.cs.w) { c => if (p.rocks(p.cs.coord(x = c, y = r).idx)) solid else blank }
            .mkString,
        )
        .mkString("\n")

  // TODO: Consider extracting from here and Day10
  case class Coord(cs: CoordSpace, idx: Int) {
    def r: Int = idx / cs.w
    def c: Int = idx % cs.w
    def rc: (Int, Int) = (r, c)
  }

  // TODO: Consider extracting from here and Day10
  case class CoordSpace(w: Int, h: Int) {
    val size: Int = w * h

    def coord(i: Int): Coord = Coord(this, i)
    def coord(x: Int, y: Int): Coord = Coord(this, w * y + x)

    def rowNums: Range = 0 until h
    def colNums: Range = 0 until w

    def rowCoords(r: Int): Iterator[Coord] = colNums.iterator.map(coord(_, r))
    def colCoords(c: Int): Iterator[Coord] = rowNums.iterator.map(coord(c, _))
  }

  def parseInput(s: String): Input > Effects = {
    import cats.parse.{Numbers as N, Rfc5234 as R, Parser as P}
    import cats.syntax.all.*

    val nl = P.char('\n')

    val pixel = P.char('#').as(true) | P.char('.').as(false)
    val pattern = pixel.rep
      .repSep(nl)
      .map { rows =>
        val h = rows.size
        val w = rows.head.size
        val rocks = rows.flatten.zipWithIndex.foldl(BitSet.empty) {
          case (rocks, (true, i)) => rocks + i
          case (rocks, _)         => rocks
        }
        Pattern(CoordSpace(w, h), rocks)
      }

    val patterns = pattern.repSep((nl ~ nl).backtrack) <* nl.rep0

    val input = patterns.map(Input.apply)

    val result = input.parseAll(s)

    result
      .leftMap(e => s"$e ${e.input.map(s => s.slice(e.failedAtOffset, e.failedAtOffset + 32))}")
      .fold(Tries.fail, identity)
  }

  def part1(input: Input): String > Effects = {
    // println(input.show)
    val result = input.patterns
      .map(p => findReflection(p).getOrElse(sys.error(s"No reflections found in \n${p.show}")))
      .map(value)
      .sumAll
    s"$result"
  }

  def part2(input: Input): String > Effects = {
    val result = input.patterns
      .map { p =>
        val r0 = findReflection(p).get
        (0 until p.cs.size).toList
          .collectFirstSome { i =>
            val p1 = p.copy(rocks = p.rocks.xor(BitSet.empty + i))
            findReflection(p1, ignore = r0.some)
          }
          .getOrElse(sys.error(s"No reflections found in \n${p.show}"))
      }
      .map(value)
      .sumAll
    s"$result"
  }

  private def findReflection(p: Pattern, ignore: Option[Either[Int, Int]] = None): Option[Either[Int, Int]] = {
    def scan1(line: Seq[Coord], ignore: Option[Int]): Set[Int] = {
      val rocks = line.map(c => p.rocks(c.idx)).map(if (_) solid else blank)
      // println(s"${rocks.mkString} …")
      val mirrorOrdinates = (1 until rocks.size).iterator
        .filter(i => !ignore.contains(i))
        .filter { i =>
          val (part1, part2) = rocks.splitAt(i)
          val zipped         = part1.reverse zip part2
          val mirrored       = zipped.forall((r1, r2) => r1 == r2)
          // println(s"${part1.mkString} | ${part2.mkString} $mirrored")
          mirrored
        }
        .toSet
      // println(s"${rocks.mkString} ${mirrorOrdinates.toList.sorted.mkString_("[", ",", "]")}")
      mirrorOrdinates
    }

    def scan(lines: Seq[Seq[Coord]], ignore: Option[Int]): Option[Int] = {
      lines.map(scan1(_, ignore)).reduce(_ intersect _).headOption
    }

    def vReflection =
      scan(p.cs.rowNums.map(r => p.cs.rowCoords(r).toList), ignore.flatMap(_.left.toOption)).map(_.asLeft)
    def hReflection = scan(p.cs.colNums.map(c => p.cs.colCoords(c).toList), ignore.flatMap(_.toOption)).map(_.asRight)
    vReflection.orElse(hReflection)
  }

  private def value(reflection: Either[Int, Int]) = reflection.map(_ * 100).merge
}
