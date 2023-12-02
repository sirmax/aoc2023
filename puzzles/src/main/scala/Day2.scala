import kyo.>
import kyo.App.Effects

object Day2 extends util.AocApp(2023, 2) {
  private def parse(input: List[String]) = {
    import fastparse._, NoWhitespace._
    def digit[$: P] = CharIn("0-9")
    def int[$: P]: P[Int] = P(digit.rep(min = 1).!.map(_.toInt))

    def red[$: P] = P("red").map(_ => Color.red)
    def green[$: P] = P("green").map(_ => Color.green)
    def blue[$: P] = P("blue").map(_ => Color.blue)

    def color[$: P]: P[Color] = P(red | green | blue)
    def gameRound[$: P] = (int ~ " " ~ color).map(_.swap).rep(sep = ", ").map(_.toMap)

    def game[$: P] = P("Game " ~ int ~ ": " ~ gameRound.rep(sep = "; ")).map(Game.apply)

    input.map(fastparse.parse(_, game).get.value)
  }

  case class Game(n: Int, rounds: Seq[Map[Color, Int]])
  enum Color:
    case red, green, blue

  def part1(input: List[String]): String > Effects = {
    val in: List[Game] = parse(input)
    // only 12 red cubes, 13 green cubes, and 14 blue cubes
    def valid(r: Map[Color, Int]) = {
      val rr = r.withDefaultValue(0)
      rr(Color.red) <= 12 && rr(Color.green) <= 13 && rr(Color.blue) <= 14
    }

    val result = in.filter(_.rounds.forall(valid)).map(_.n).sum
    s"$result"
  }

  def part2(input: List[String]): String > Effects = {
    val in: List[Game] = parse(input)

    def minCubesNeeded(r1: Map[Color, Int], r2: Map[Color, Int]) = {
      Color.values.map(c => c -> Math.max(r1.getOrElse(c, 0), r2.getOrElse(c, 0))).toMap
    }
    def power(r: Map[Color, Int]) = {
      Color.values.map(r.getOrElse(_, 0)).product.toLong
    }
    val result = in
      .map(_.rounds.reduce(minCubesNeeded))
      .map(power)
      .sum
    s"$result"
  }
}
