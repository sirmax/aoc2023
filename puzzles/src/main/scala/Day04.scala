import kyo.>
import kyo.apps.App.Effects
import kyo.tries.Tries

object Day04 extends util.AocApp(2023, 4) {
  case class Input(cards: Seq[Card])

  case class Card(n: Int, winning: Seq[Int], have: Seq[Int]) {
    def nMatched: Int = have.intersect(winning).size
  }

  def parseInput(s: String): Input > Tries = {
    parseInputCatsparse(s)
//    parseInputFastparse(s)
  }

  def parseInputCatsparse(s: String): Input > Tries = {
    import cats.parse.{Numbers => N, Parser => P, Rfc5234 => R}
    import cats.parse.Rfc5234
    import cats.syntax.all.*

    val sps  = R.sp.rep.void
    val int  = N.digits.map(_.toInt).surroundedBy(R.sp.rep0)
    val ints = int.rep
    val pipe = P.char('|').surroundedBy(R.sp.rep0).void
    val card = (P.string("Card"), int, P.char(':'), ints, P.char('|'), ints).tupled
      .map((_, n, _, winning, _, have) => Card(n, winning.toList, have.toList))

    val input  = (card.repSep(P.char('\n')) <* P.char('\n').rep0).map(x => Input(x.toList))
    val result = input.parseAll(s)

    result.leftMap(_.toString()).fold(Tries.fail, identity)
  }

  def parseInputFastparse(s: String): Input > Tries = {
    import fastparse.*, SingleLineWhitespace.*

    def int[$: P] = CharsWhileIn("0-9").!.map(_.toInt)
    def card[$: P] = P("Card" ~ int ~ ":" ~ int.rep ~ "|" ~ int.rep).map(Card.apply)
    def input[$: P] = P(card.rep(sep = "\n", min = 1)).map(Input.apply)

    val parsed = parse(s, input)
    Tries.catching(parsed.get.value)
  }

  def part1(input: Input): String > Effects = {
    val points = 0 +: Array.iterate(1, input.cards.head.winning.size)(_ * 2)
    input.cards
      .map(c => points(c.nMatched))
      .sum
      .toString
  }

  def part2(input: Input): String > Effects = {
    case class State(score: Long, bonus: List[Long])

    input.cards
      .foldLeft(State(0, List.empty)) { (s, c) =>
        val nCards = 1L + s.bonus.headOption.getOrElse(0L)
        val score  = s.score + nCards
        val bonus = List
          .fill(c.nMatched)(nCards)
          .zipAll(s.bonus.drop(1), 0L, 0L)
          .map(_ + _)
        State(score, bonus)
      }
      .score
      .toString
  }
}
