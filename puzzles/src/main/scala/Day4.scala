import kyo.>
import kyo.App.Effects

object Day4 extends util.AocApp(2023, 4) {
  case class Input(cards: Seq[Card])

  case class Card(n: Int, winning: Seq[Int], have: Seq[Int]) {
    def nMatched: Int = have.intersect(winning).size
  }

  def parseInput(s: String): Input = {
    parseInputCatsparse(s)
  }

  def parseInputCatsparse(s: String): Input = {
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
    result.fold(x => sys.error(x.toString()), identity)
  }

  def parseInputFastparse(s: String): Input = {
    import fastparse.*, SingleLineWhitespace.*

    def int[$: P] = CharsWhileIn("0-9").!.map(_.toInt)
    def card[$: P] = P("Card" ~ int ~ ":" ~ int.rep ~ "|" ~ int.rep).map(Card.apply)
    def input[$: P] = P(card.rep(sep = "\n")).map(Input.apply)

    val parsed = parse(s, input)
    parsed.get.value
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
