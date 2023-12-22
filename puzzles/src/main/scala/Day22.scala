import aoc.cartesian.CoordSpace
import cats.Show
import cats.data.NonEmptyList
import cats.syntax.all.*
import kyo.>
import kyo.apps.App.Effects
import kyo.tries.Tries

import scala.annotation.tailrec
import scala.collection.immutable.{BitSet, SortedSet}

object Day22 extends util.AocApp(2023, 22) {
  case class Input(cs: CoordSpace, bricks: List[Brick])

  /** @param id unique identifier of a brick, needed for SortedSet[Brick] and for brute-forcing `part2`. */
  case class Brick(footprint: BitSet, zB: Int, h: Int, id: Int) {
    def zT: Int = zB + h - 1

    def overlaps(that: Brick): Boolean = this.footprint.exists(that.footprint)
  }

  def parseInput(s: String): Input > Effects = {
    import cats.parse.{Numbers as N, Rfc5234 as R, Parser as P}

    val int      = R.digit.rep.string.map(_.toInt)
    val cmm      = P.char(',')
    val tld      = P.char('~')
    val brickRaw = (int <* cmm, int <* cmm, int <* tld, int <* cmm, int <* cmm, int).tupled

    val input = (brickRaw.repSep(R.lf).map(_.toList) <* R.lf.?).map { bs =>
      assert(bs.map(_._1).min == 0)
      assert(bs.map(_._2).min == 0)
      val xMax = bs.map(_._4).max
      val yMax = bs.map(_._5).max
      val cs   = CoordSpace(xMax + 1, yMax + 1)

      val bricks = bs.zipWithIndex.map {
        case ((x1, y1, z1, x2, y2, z2), id) =>
          assert(x1 <= x1)
          assert(y1 <= y1)
          assert(z1 <= z1)
          Brick(
            footprint = BitSet.fromSpecific(((x1 to x2).toList, (y1 to y2).toList).mapN((x, y) => cs.coord(x, y).idx)),
            zB = z1,
            h = z2 - z1 + 1,
            id = id,
          )
      }

      Input(cs, bricks)
    }

    input.parseAll(s).leftMap(e => throw new IllegalArgumentException(s"Failed to parse\n${e.show}")).merge
  }

  def part1(input: Input): String > Effects = {
    // println(input)
    val dropped = drop(input.bricks)

    val byZT = dropped.groupBy(_.zT).withDefaultValue(List.empty)
    val byZB = dropped.groupBy(_.zB).withDefaultValue(List.empty)

    val result = dropped.count { b =>
      val supported  = byZB(b.zT + 1).filter(_.overlaps(b))
      val neighbours = byZT(b.zT).toSet - b
      val canRemove  = supported.forall(b1 => neighbours.exists(_.overlaps(b1)))
      canRemove
    }

    s"$result"
  }

  def part2(input: Input): String > Effects = {
    val dropped = drop(input.bricks)
    val byZT    = dropped.groupBy(_.zT).withDefaultValue(List.empty)
    val byZB    = dropped.groupBy(_.zB).withDefaultValue(List.empty)

    // TODO: rewrite into a non-brute-forcing algo.
    val all = dropped.toSet
    val result = all.iterator.map { b =>
      val bs = all - b
      (bs -- drop(bs.toList).toSet).size
    }.sum
    s"$result"
  }

  private def drop(bs: List[Brick]): List[Brick] = {
    @tailrec def recur(todo: List[Brick], dropped: SortedSet[Brick]): List[Brick] = todo match {
      case b :: bs =>
        // Find the block (or ground) to fall onto, calculate new Z-bottom from it.
        val zB = dropped.find(d => d.zT < b.zB && b.overlaps(d)).map(_.zT + 1).getOrElse(1)
        val b1 = b.copy(zB = zB)
        recur(bs, dropped + b1)

      case Nil => dropped.toList
    }
    val result =
      recur(bs.sortBy(_.zB), SortedSet.empty(Ordering.by((b: Brick) => (b.zT, b.id)).reverse))
    assert(result.size == bs.size)
    result
  }
}
