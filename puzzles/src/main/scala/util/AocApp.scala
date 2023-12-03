package util

import kyo.>
import kyo.App
import kyo.App.Effects
import kyo.consoles.Consoles
import kyo.direct.*
import kyo.ios.IOs

abstract class AocApp(year: Int, day: Int) extends App {
  def run(args: List[String]): Unit > Effects = defer {
    val maybeConfig = await(IOs(scopt.OParser.parse(oParser, args, AocApp.Config())))

    maybeConfig match
      case Some(config) => {
        val fullInputName = s"$year/Day$day.${config.inputName}.txt"
        val input         = await(IOs(parseInputStream(this.getClass.getResourceAsStream(fullInputName))))
        if (config.parts(AocApp.Part.Part1)) {
          await(Consoles.println(s"Part 1: ${part1(input)}"))
        }
        if (config.parts(AocApp.Part.Part2)) {
          await(Consoles.println(s"Part 2: ${part2(input)}"))
        }
      }
      case None =>
  }

  type Input

  def parseInputStream(is: java.io.InputStream): Input = {
    parseInput(io.Source.fromInputStream(is, "UTF-8"))
  }

  def parseInput(s: io.Source): Input
  def part1(input: Input): String > Effects
  def part2(input: Input): String > Effects

  private def oParser = {
    given rPart: scopt.Read[AocApp.Part] = scopt.Read.reads(s => AocApp.Part.valueOf(s"Part$s"))

    val builder = scopt.OParser.builder[AocApp.Config]
    import builder._
    scopt.OParser.sequence(
      help('h', "help").text("prints this usage text"),
      opt[Seq[AocApp.Part]]('p', "part")
        .optional()
        .text("A part to run. Can be `1`, `2`, or `1,2` together. Defaults to `1,2`")
        .action((x, c) => c.copy(parts = x.toSet)),
      arg[String]("name")
        .text("Makes the app load `DayN.<name>.txt` input file. Defaults to `main`.")
        .optional()
        .validate(x => Either.cond(x.trim.nonEmpty, x.trim, "empty name"))
        .action((x, c) => c.copy(inputName = x)),
    )
  }
}

object AocApp {
  case class Config(parts: Set[Part] = Set(Part.Part1, Part.Part2), inputName: String = "main")

  enum Part:
    case Part1, Part2
}
