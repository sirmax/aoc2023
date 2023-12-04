package util

import kyo.*
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
        val fullInputName   = s"$year/Day$day.${config.inputName}.txt"
        val fullAnswersName = s"$year/Day$day.${config.inputName}.answers.txt"
        val input           = await(IOs(parseInput(io.Source.fromResource(fullInputName).mkString)))

        val answers = await {
          val run = IOs(io.Source.fromResource(fullAnswersName).getLines().take(2).toList)
          kyo.tries.Tries
            .handle(run) { case e: java.io.FileNotFoundException => List.empty }
            .map(_.padTo(2, ""))
        }

        if (config.parts(AocApp.Part.Part1)) {
          await(runPart(1, answers(0), input, part1))
        }
        if (config.parts(AocApp.Part.Part2)) {
          await(runPart(2, answers(1), input, part2))
        }
      }
      case None =>
  }

  private def runPart[S](nPart: Int, answer: String, input: Input, run: Input => String > S): Unit > S with Consoles =
    defer {
      val result = await(run(input))
      val (resultIcon, mismatchInfo) = answer match {
        case ""       => (" ", "")
        case `result` => ("*", "")
        case other    => ("!", s", expected $other")
      }
      await(Consoles.println(s"$resultIcon Part $nPart: $result$mismatchInfo"))
    }

  type Input

  def parseInput(s: String): Input
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
