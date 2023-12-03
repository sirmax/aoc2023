import kyo.*
import kyo.consoles.*
import kyo.direct.*

object Day1 extends util.AocApp(2023, 1) {
  override type Input = List[String]

  def parseInput(s: String): List[String] = s.linesIterator.toList

  def part1(input: List[String]): String = {
    val rFirst = "(\\d)".r
    val rLast  = ".*(\\d)".r
    input
      .map { s =>
        val first = rFirst.findFirstIn(s).get
        val last  = rLast.findFirstMatchIn(s).map(_.group(1)).get
        s"$first$last".toInt
      }
      .sum
      .toString
  }

  def part2(input: List[String]): String = {
    val pattern = "(\\d|one|two|three|four|five|six|seven|eight|nine)"
    val rFirst  = s"$pattern".r
    val rLast   = s".*$pattern".r
    val words = List("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
      .zip(Iterator.from(1))
      .toMap
    def toInt(s: String) = words.getOrElse(s, s.toInt)

    input
      .map { s =>
        val first = rFirst.findFirstIn(s).map(toInt).get
        val last  = rLast.findFirstMatchIn(s).map(_.group(1)).map(toInt).get
        s"$first$last".toInt
      }
      .sum
      .toString
  }
}
