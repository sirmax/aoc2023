import cats.data.NonEmptyList
import cats.syntax.all.*
import kyo.>
import kyo.apps.App.Effects
import kyo.tries.Tries

object Day07 extends util.AocApp(2023, 7) {
  given orderFromOrdering[A: Ordering]: cats.kernel.Order[A] = cats.kernel.Order.fromOrdering(implicitly)

  private def reverse[A](using oa: Ordering[A]): Ordering[A] = oa.reverse
  private def reverse[A](using oa: cats.kernel.Order[A]): cats.kernel.Order[A] = cats.kernel.Order.reverse(oa)

  case class Input(handBids: NonEmptyList[HandBid])

  case class HandBid(hand: Hand, bid: Long)

  case class Hand(cards: NonEmptyList[Card]) {
//    override def toString: String = s"${cards.toList.mkString("")}:$handType1(${cardsSorted.toList.mkString("")})"
//    override def toString: String = s"${cards.toList.mkString("")}:$handType2(${cardsSorted2.mkString("")})"

    def cardsSorted = cards.sorted(reverse)
    def cardsSorted2 = {
      val (js, rest) = cards.toList.partition(_ == Card.J)
      js ++ rest.sorted(reverse)
    }

    val handType1: HandType = {
      cards.toList.groupBy(identity).values.map(_.size).toList.sorted(reverse) match {
        case 5 :: _      => HandType.`5`
        case 4 :: _      => HandType.`4`
        case 3 :: 2 :: _ => HandType.`3_2`
        case 3 :: _      => HandType.`3`
        case 2 :: 2 :: _ => HandType.`2_2`
        case 2 :: _      => HandType.`2`
        case _           => HandType.`1`
      }
    }

    val handType2: HandType = {
      val (js, rest) = cards.toList.partition(_ == Card.J)
      val cardCounts = rest.groupBy(identity).values.map(_.size).toList.sorted(reverse)
      val adjustedCounts = cardCounts match
        case n :: rest => (n + js.size) :: rest
        case Nil       => js.size :: Nil
      adjustedCounts match {
        case 5 :: _      => HandType.`5`
        case 4 :: _      => HandType.`4`
        case 3 :: 2 :: _ => HandType.`3_2`
        case 3 :: _      => HandType.`3`
        case 2 :: 2 :: _ => HandType.`2_2`
        case 2 :: _      => HandType.`2`
        case _           => HandType.`1`
      }
    }
  }

  object Hand {
    import Ordering.Implicits.seqOrdering

    given ordering1: Ordering[Hand] = Ordering.by(h => (h.handType1, h.cards.toList))
    given ordering2: Ordering[Hand] =
      Ordering.by(h => (h.handType2, h.cards.map(_.some.filter(_ != Card.J).map(_.ordinal).getOrElse(-1)).toList))
  }

  enum Card:
    case `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9`, T, J, Q, K, A

  object Card:
    given Ordering[Card] = Ordering.by(_.ordinal)

  enum HandType:
    case `1`, `2`, `2_2`, `3`, `3_2`, `4`, `5`

  object HandType:
    given Ordering[HandType] = Ordering.by(_.ordinal)

  def parseInput(s: String): Input > Effects = {
    import cats.parse.{Numbers => N, Parser => P, Rfc5234 => R}
    import cats.parse.Rfc5234
    import cats.syntax.all.*

    val sps   = R.sp.rep.void
    val long  = N.digits.map(_.toLong).surroundedBy(R.sp.rep0)
    val longs = long.rep
    val nl    = P.char('\n')

    val card    = P.charIn(Card.values.map(_.toString.charAt(0))).map(c => Card.valueOf(c.toString))
    val hand    = card.rep(5).map(Hand.apply)
    val handBid = (hand, R.sp, long).mapN((hand, _, bid) => HandBid(hand, bid))
    val input   = (handBid.repSep(nl) <* nl.?).map(Input.apply)

    val result = input.parseAll(s)
    result
      .leftMap(e => s"$e ${e.input.map(s => s.slice(e.failedAtOffset, e.failedAtOffset + 32))}")
      .fold(Tries.fail, identity)
  }

  def part1(input: Input): String > Effects = {
    val result = input.handBids.toList
      .sortBy(_.hand)(Hand.ordering1)
      .iterator
      .zip(Iterator.from(1))
//      .tapEach(println)
      .map((hb, rank) => hb.bid * rank)
      .sum
    s"$result"
  }

  def part2(input: Input): String > Effects = {
    val result = input.handBids.toList
      .sortBy(_.hand)(Hand.ordering2)
      .iterator
      .zip(Iterator.from(1))
//      .tapEach(println)
      .map((hb, rank) => hb.bid * rank)
      .sum
    s"$result"
  }
}
