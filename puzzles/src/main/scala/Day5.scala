import cats.data.NonEmptyList
import cats.syntax.all.*
import kyo.*
import kyo.apps.App.Effects
import kyo.tries.Tries

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object Day5 extends util.AocApp(2023, 5) {
  case class Input(seeds: NonEmptyList[Long], mappings: NonEmptyList[Mapping])
  case class Mapping(name: String, ranges: NonEmptyList[Range])
  case class Range(dst: Long, src: Long, length: Long) {
    override def toString: String = s"$productPrefix($src -> $dst, $length)"

    def srcSpan: Span = Span(src, src + length)
    def dstSpan: Span = Span(dst, dst + length)
  }

  def parseInput(s: String): Input > Tries = {
    import cats.parse.{Numbers => N, Parser => P, Rfc5234 => R}
    import cats.parse.Rfc5234
    import cats.syntax.all.*

    val sps   = R.sp.rep.void
    val long  = N.digits.map(_.toLong).surroundedBy(R.sp.rep0)
    val longs = long.rep
    val nl    = P.char('\n')

    val seeds         = P.string("seeds:") *> longs <* nl
    val range         = (long, long, long).tupled.map(Range.apply)
    val ranges        = range.repSep(nl)
    val mappingHeader = R.vchar.rep.string <* P.string(" map:") <* nl
    val mapping       = (mappingHeader ~ ranges).map(Mapping.apply)
    val mappings      = (mapping <* nl.?).repSep(nl)

    val input  = (seeds, nl, mappings).tupled.map((seeds, _, mappings) => Input(seeds, mappings))
    val result = input.parseAll(s)

    result
      .leftMap(e => s"$e ${e.input.map(s => s.slice(e.failedAtOffset, e.failedAtOffset + 32))}")
      .fold(Tries.fail, identity)
  }

  def part1(input: Input): String > Effects = {
    val result = input.seeds.map { s =>
      input.mappings.foldLeft(s) { (n, mapping) =>
        mapping.ranges
          .collectFirstSome { range =>
            val offset = n - range.src
            Option.when(offset >= 0 && offset < range.length)(range.dst + offset)
          }
          .getOrElse(n)
      }
    }.minimum
    s"$result"
  }

  case class Span(from: Long, until: Long) {
    def nonEmpty: Boolean = from < until

    def length: Long = if (nonEmpty) until - from else 0

    def offset(n: Long): Span = Span(from + n, until + n)

    def subtract(that: Span): List[Span] = {
      List(Span(from, Math.min(until, that.from)), Span(Math.max(that.until, from), until))
        .filter(_.nonEmpty)
    }

    def intersect(that: Span): Option[Span] = {
      Span(Math.max(from, that.from), Math.min(until, that.until)).some.filter(_.nonEmpty)
    }

    def union(that: Span): Option[Span] = {
      val List(s1, s2) = List(this, that).sortBy(_.from)
      Option.when(s1.until >= s2.from)(Span(s1.from, Math.max(s1.until, s2.until)))
    }
  }

  object Span {
    def merge(spans: List[Span]): List[Span] = {
      @tailrec
      def recur(sorted: List[Span], acc: ListBuffer[Span]): List[Span] = {
        sorted match {
          case s1 :: s2 :: rest =>
            s1.union(s2) match {
              case Some(s) => recur(s :: rest, acc)
              case None    => recur(s2 :: rest, acc.appended(s1))
            }
          case s :: Nil => acc.appended(s).toList
          case Nil      => acc.toList
        }
      }
      recur(spans.sortBy(_.from), ListBuffer.empty)
    }
  }

  def part2(input: Input): String > Effects = {

    def map(span: Span, range: Range): List[Either[Span, Span]] = {
      val srcSpan = Span(range.src, range.src + range.length)

      val mapped   = span.intersect(srcSpan).map(_.offset(range.dst - range.src))
      val unmapped = span.subtract(srcSpan)
      mapped.map(_.asRight) ++: unmapped.map(_.asLeft)
    }

    val spans = input.seeds
      .grouped(2)
      .map(_.toList)
      .map { x =>
        val List(start, n) = x
        Span(start, start + n)
      }
      .toList

    val result = input.mappings
      .foldLeft(spans.sortBy(_.from)) { (spans, mapping) =>
//        println(s"${mapping.name}\n  ranges=${mapping.ranges}\n   spans=$spans")
        val (mapped, unmapped) = mapping.ranges.foldLeft((List.empty[Span], spans)) {
          case ((mapped, unmapped), range) =>
            val (unmapped1, mapped1) = unmapped.flatMap(map(_, range)).partitionMap(identity)
            (mapped ++ mapped1, unmapped1)
        }
        val res = Span.merge(mapped ++ unmapped)
//        println(s"  result=$res")
        res
      }
      .minBy(_.from)
      .from
    s"$result"
  }
}
