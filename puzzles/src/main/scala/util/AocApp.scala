package util

import kyo.*
import kyo.apps.App
import kyo.apps.App.Effects
import kyo.clocks.Clocks
import kyo.consoles.Consoles
import kyo.direct.*
import kyo.ios.IOs

import java.nio.file.{Files, Path, Paths}
import scala.concurrent.duration.FiniteDuration

abstract class AocApp(year: Int, day: Int) extends App {
  def run: Unit > Effects = defer {
    val maybeConfig = await(IOs(scopt.OParser.parse(oParser, args, AocApp.Config())))

    maybeConfig match
      case Some(config) =>
        val inputsRoot      = config.inputsRoot.toAbsolutePath
        val fullInputPath   = inputsRoot.resolve(s"$year/Day$day.${config.inputName}.txt")
        val fullAnswersPath = inputsRoot.resolve(s"$year/Day$day.${config.inputName}.answers.txt")

        val (input, parseTime) = await(timed(IOs(parseInput(Files.readString(fullInputPath)))))
        await(Consoles.println(s"Parse time: ${parseTime.toMillis.toDouble / 1_000}s"))

        val answers = await {
          val run = IOs(Files.readString(fullAnswersPath).linesIterator.take(2).toList)
          kyo.tries.Tries
            .handle(run) { case e: java.nio.file.NoSuchFileException => List.empty }
            .map(_.padTo(2, ""))
        }

        if (config.parts(AocApp.Part.Part1)) {
          await(runPart(1, answers(0), input, part1))
        }
        if (config.parts(AocApp.Part.Part2)) {
          await(runPart(2, answers(1), input, part2))
        }
      case None =>
  }

  private def runPart[S](nPart: Int, answer: String, input: Input, run: Input => String > S): Unit > S with Consoles =
    defer {
      val (result, time) = await(timed(IOs(run(input))))
      val (resultIcon, mismatchInfo) = answer match {
        case ""       => (" ", "")
        case `result` => ("*", "")
        case other    => ("!", s", expected $other")
      }
      val timeSpent = s"; time spent: ${time.toMillis.toDouble / 1_000}s"
      await(Consoles.println(s"$resultIcon Part $nPart: $result$mismatchInfo$timeSpent"))
    }

  private def timed[A, S](run: A > S): (A, FiniteDuration) > (S & IOs) = defer {
    val t0    = await(Clocks.now)
    val a     = await(run)
    val t1    = await(Clocks.now)
    val dJava = java.time.Duration.between(t0, t1)
    val d     = FiniteDuration(dJava.toNanos, scala.concurrent.duration.NANOSECONDS)
    (a, d)
  }

  type Input

  def parseInput(s: String): Input > Effects
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
      opt[java.nio.file.Path]('i', "input")
        .optional()
        .text(s"""A directory with input files: <input>/20XX/Day*.txt
                 |Defaults to AOC_INPUTS environment variable or to "${AocApp.Config().inputsRoot}".
                 |""".stripMargin)
        .withFallback { () =>
          sys.env.get("AOC_INPUTS").map(Paths.get(_)).getOrElse(AocApp.Config().inputsRoot)
        }
        .validate(x => Either.cond(Files.isDirectory(x), (), s"${x.toAbsolutePath} is not a valid directory"))
        .action((x, c) => c.copy(inputsRoot = x)),
      arg[String]("name")
        .text("Makes the app load `DayN.<name>.txt` input file. Defaults to `main`.")
        .optional()
        .validate(x => Either.cond(x.trim.nonEmpty, x.trim, "empty name"))
        .action((x, c) => c.copy(inputName = x)),
    )
  }
}

object AocApp {
  case class Config(
    parts: Set[Part] = Set(Part.Part1, Part.Part2),
    inputName: String = "main",
    inputsRoot: Path = Paths.get("inputs"),
  )

  enum Part:
    case Part1, Part2
}
