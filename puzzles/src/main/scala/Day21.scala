import aoc.cartesian.{CoordSpace, Direction}
import cats.Show
import cats.data.NonEmptyList
import cats.syntax.all.*
import kyo.>
import kyo.apps.App.Effects
import kyo.tries.Tries

import scala.collection.immutable.BitSet

object Day21 extends util.AocApp(2023, 21) {
  case class Input(garden: Garden, targetSteps: Int) {
    override def toString: String = s"targetSteps=$targetSteps\n$garden"
  }

  case class Garden(cs: CoordSpace, rocks: BitSet, start: BitSet) {
    override def toString: String = cs.render(".", List(("S", start), ("#", rocks)))

    def grow(n: Int): Garden = {
      val k   = 1 + n * 2 // center + sides
      val cs1 = CoordSpace(cs.w * k, cs.h * k)
      val rockIdx1 = for {
        idx <- rocks.toList
        c    = cs.coord(idx)
        kw  <- 0 until k
        kh  <- 0 until k
        idx1 = cs1.coord(c.x + kw * cs.w, c.y + kh * cs.h).idx
      } yield idx1
      val rocks1 = BitSet.fromSpecific(rockIdx1)
      val start1 = start.map { idx =>
        val c = cs.coord(idx)
        cs1.coord(c.x + cs.w * n, c.y + cs.h * n).idx
      }
      Garden(cs1, rocks1, start1)
    }
  }

  object Input {
    given Show[Input] = Show.fromToString
  }

  def parseInput(s: String): Input > Effects = {
    import cats.parse.{Numbers as N, Rfc5234 as R, Parser as P}

    val int = R.digit.rep.string.map(_.toInt)

    val garden = P.charIn(".#S").rep.repSep(R.lf).map { rows =>
      val cs = CoordSpace(w = rows.head.length, h = rows.length)
      def all = rows.iterator.flatMap(_.iterator)
      val rocks = BitSet.fromSpecific(all.zipWithIndex.collect { case ('#', i) => i })
      val start = BitSet(all.indexOf('S'))
      Garden(cs, rocks, start)
    }

    val input = (garden <* R.lf, int <* R.lf.?).mapN(Input.apply)

    input.parseAll(s).leftMap(e => throw new IllegalArgumentException(s"Failed to parse\n${e.show}")).merge
  }

  def part1(input: Input): String > Effects = {
    // println(input.show)
    val garden = input.garden
    def steps = LazyList.iterate(Mask.start(garden.start), 1 + input.targetSteps)(step(garden, _))
    val result = steps.last.thisStep.size
    s"$result"
  }

  def part2(input: Input): String > Effects = {
    // The actual assignment exhibits a rhombus pattern, that seems to repeat itself, at least when growing
    // by cs.w + cs.w / 2. I'm not sure if the "remainder" part has some significance, but the target number
    // of steps is, coincidentally, cs.w / 2.
    //
    // So, to solve this task we simulate only `N * cs.w + remainder` steps, where N has the same parity as
    // the actual N full cs.w in the target steps, and remainder is the same as in the target.
    //
    // With even N, and odd target steps we would get something like this (each letter represents one garden):
    //
    //   abc
    //  adEfc
    //  gEOEh
    //  jkElm
    //   jnm
    //
    // Capital 'E' and 'O' represent completely visited gardens at even and odd steps respectively.
    // Small letters represent partially visited gardens. Note the repeating ones. The diagram above
    // shows repeating 'a', 'c', 'j', and 'm', however the inner diagonal cells: 'd', 'f', 'k', 'l',
    // also repeat with more steps made.

    val w0     = input.garden.cs.w
    val garden = input.garden.grow(2) // to accommodate the needed variants when simulating.
    val target = 26501365
    // val target    = w0 * 2 + w0 / 2 // for debugging with the reference
    val remainder = target % w0
    val nFull     = target / w0

    assume(nFull % 2 == 0, s"Expecting an even number of full-width steps, but got $target / $w0 = $nFull")

    // There should be come simple formula, but I'd rather calculate these sums iteratively :garold:
    val nFullOdd  = 1 /* for the center */ + (2L until nFull by 2).map(_ * 4).sum
    val nFullEven = (1L until nFull by 2).map(_ * 4).sum

    val nInnerCorners = nFull - 1
    val nOuterCorners = nFull

    // println(s"$nFullEven + $nFullOdd = ${nFullEven + nFullOdd}")
    // println(s"nFullEven=$nFullEven nFullOdd=$nFullOdd nInnerCorners=$nInnerCorners nOuterCorners=$nOuterCorners")

    val cs    = garden.cs
    val start = garden.start

    def steps = LazyList
      .iterate(Mask.start(start))(step(garden, _))
      .zipWithIndex
      .drop(1)
      // .tapEach { (m, i) =>
      //   if (i % w0 == w0 / 2 || i % w0 == 0 || m.edge.isEmpty) {
      //     def plot = cs.render(" ", List("█" -> m.edge, "░" -> m.thisStep) :+ (".", garden.rocks))
      //     println(s"\n$i = (${i / w0}*$w0 + ${i % w0}) ----\n$plot")
      //   }
      // }
      .map(_._1)

    val reference = steps(2 * w0 + remainder - 1)
    // println(cs.render(" ", List(/*"█" -> reference.edge, */"░" -> reference.thisStep) :+ (".", garden.rocks)))

    /** Extracts a number of visited cells from a target plot (one of the original garden clones).
      * `px` and `py` refer to the plot location, (0, 0) being the original – central one.
      */
    def extract(px: Int, py: Int): Long = {
      val plotOffset = (cs.w / w0) / 2
      val dx         = (plotOffset + px) * w0
      val dy         = (plotOffset + py) * input.garden.cs.h
      val mask       = BitSet.fromSpecific(input.garden.cs.coords.map(c => cs.coord(c.x + dx, c.y + dy).idx))
      val extracted  = mask intersect reference.thisStep

      // println(cs.render(" ", List("█" -> extracted, "░" -> reference.thisStep) :+ (".", garden.rocks)))
      extracted.size.toLong
    }

    val result = List(
      nFullOdd * extract(0, 0),
      nFullEven * extract(1, 0),
      nInnerCorners * extract(-1, -1),
      nInnerCorners * extract(1, -1),
      nInnerCorners * extract(-1, 1),
      nInnerCorners * extract(1, 1),
      nOuterCorners * extract(-2, -1),
      nOuterCorners * extract(2, -1),
      nOuterCorners * extract(-2, 1),
      nOuterCorners * extract(2, 1),
      extract(-2, 0),
      extract(2, 0),
      extract(0, -2),
      extract(0, 2),
    ).sum

    // assert(result == reference.thisStep.size, s"$result != ${reference.thisStep.size}")
    s"$result"
  }

  private def step(garden: Garden, mask: Mask): Mask = {
    val cs = garden.cs
    val edge = mask.edge.foldLeft(BitSet.empty) { (edge, idx) =>
      val reachable = for {
        d <- Direction.values
        c <- cs.coord(idx).next(d).toList
        if !(garden.rocks(c.idx) || mask.prevStep(c.idx))
      } yield c.idx
      edge ++ reachable
    }
    Mask(edge = edge, thisStep = mask.prevStep ++ edge, prevStep = mask.thisStep)
  }

  private case class Mask(edge: BitSet, thisStep: BitSet, prevStep: BitSet)

  private object Mask {
    def start(edge: BitSet): Mask = Mask(edge = edge, thisStep = edge, prevStep = BitSet.empty)
  }
}
