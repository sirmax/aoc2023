package util

import kyo.>
import kyo.App
import kyo.App.Effects
import kyo.consoles.Consoles
import kyo.direct.*
import kyo.ios.IOs

abstract class AocApp(year: Int, day: Int) extends App {
  def run(args: List[String]): Unit > Effects = defer {
    val input = await(IOs(scala.io.Source.fromResource(s"$year/Day$day.main.txt").getLines().toList))
    await(Consoles.println(s"Part 1: ${part1(input)}"))
    await(Consoles.println(s"Part 2: ${part2(input)}"))
  }

  def part1(input: List[String]): String > Effects
  def part2(input: List[String]): String > Effects
}
