import cats.data.NonEmptyList
import cats.syntax.all.*
import kyo.*
import kyo.App.Effects
import kyo.tries.Tries

import scala.collection.mutable.ListBuffer

object Day5 extends util.AocApp(2023, 5) {
  case class Input(seeds: NonEmptyList[Long], mappings: NonEmptyList[Mapping])
  case class Mapping(name: String, ranges: NonEmptyList[Range])
  case class Range(dst: Long, src: Long, length: Long) {
    override def toString: String = s"$productPrefix($src -> $dst, $length)"
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
    def nonEmpty = from < until
    def offset(n: Long) = Span(from + n, until + n)
  }

  def part2(input: Input): String > Effects = {

    def map(span: Span, range: Range): List[Either[Span, Span]] = {
      val srcSpan    = Span(range.src, range.src + range.length)
      val srcOverlap = Span(Math.max(span.from, srcSpan.from), Math.min(span.until, srcSpan.until))

      val result = if (srcOverlap.nonEmpty) {
        val offset = range.dst - range.src
        val unmapped = List(Span(span.from, srcOverlap.from), Span(srcOverlap.until, span.until))
          .filter(_.nonEmpty)
          .map(_.asLeft)
        srcOverlap.offset(offset).asRight :: unmapped
      } else List(span.asLeft)
//      println(s"map range=$range, span=$span, srcSpan=$srcSpan, overlap=${srcOverlap.some.filter(_.nonEmpty)}, result=$result")
      result
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
        val (mapped, unmapped) = mapping.ranges.foldLeft((List.empty[Span], spans)) { case ((mapped, unmapped), range) =>
          val (unmapped1, mapped1) = unmapped.flatMap(map(_, range)).partitionMap(identity)
          (mapped ++ mapped1, unmapped1)
        }
        val sorted = (mapped ++ unmapped).sortBy(s => (s.from, s.until))
        def merge(sorted: List[Span], acc: ListBuffer[Span]): List[Span] = {
          sorted match {
            case s1 :: s2 :: rest =>
              if (s1.until >= s2.from) merge(Span(s1.from, Math.max(s1.until, s2.until)) :: rest, acc)
              else merge(s2 :: rest, acc.appended(s1))
            case s :: Nil => acc.appended(s).toList
            case Nil      => acc.toList
          }
        }
        val res = merge(sorted, ListBuffer.empty)
//        println(s"  result=$res")
        res
      }
      .minBy(_.from).from
    s"$result"
  }
}
