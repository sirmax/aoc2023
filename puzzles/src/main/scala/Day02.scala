import kyo.>
import kyo.apps.App.Effects

object Day02 extends util.AocApp(2023, 2) {
  case class Input(games: List[Game])

  def parseInput(s: String): Input = {
    import fastparse._, NoWhitespace._
    def digit[$: P] = CharIn("0-9")
    def int[$: P]: P[Int] = P(digit.rep(min = 1).!.map(_.toInt))

    def red[$: P] = P("red").map(_ => Color.red)
    def green[$: P] = P("green").map(_ => Color.green)
    def blue[$: P] = P("blue").map(_ => Color.blue)

    def color[$: P]: P[Color] = P(red | green | blue)
    def gameRound[$: P] = (int ~ " " ~ color).map(_.swap).rep(sep = ", ").map(_.toMap)

    def game[$: P] = P("Game " ~ int ~ ": " ~ gameRound.rep(sep = "; ")).map(Game.apply)

    Input(games = s.linesIterator.map(fastparse.parse(_, game).get.value).toList)
  }

  case class Game(n: Int, rounds: Seq[Map[Color, Int]])
  enum Color:
    case red, green, blue

  def part1(input: Input): String > Effects = {
    // only 12 red cubes, 13 green cubes, and 14 blue cubes
    def valid(r: Map[Color, Int]) = {
      val rr = r.withDefaultValue(0)
      rr(Color.red) <= 12 && rr(Color.green) <= 13 && rr(Color.blue) <= 14
    }

    val result = input.games.filter(_.rounds.forall(valid)).map(_.n).sum
    s"$result"
  }

  def part2(input: Input): String > Effects = {
    def minCubesNeeded(r1: Map[Color, Int], r2: Map[Color, Int]) = {
      Color.values.map(c => c -> Math.max(r1.getOrElse(c, 0), r2.getOrElse(c, 0))).toMap
    }
    def power(r: Map[Color, Int]) = {
      Color.values.map(r.getOrElse(_, 0)).product.toLong
    }
    val result = input.games
      .map(_.rounds.reduce(minCubesNeeded))
      .map(power)
      .sum
    s"$result"
  }
}
