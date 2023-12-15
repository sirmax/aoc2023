import cats.Show
import cats.data.NonEmptyList
import cats.syntax.all.*
import kyo.>
import kyo.apps.App.Effects
import kyo.tries.Tries

import scala.collection.immutable.Queue

object Day15 extends util.AocApp(2023, 15) {
  case class Input(steps: NonEmptyList[String])

  object Input:
    given Show[Input] = _.steps.mkString_(", ")

  def parseInput(s: String): Input > Effects = {
    import cats.parse.{Numbers as N, Rfc5234 as R, Parser as P}
    import cats.syntax.all.*

    val nl  = P.char('\n')
    val `,` = P.char(',')

    val input = (P.charsWhile(c => c != '\n' && c != ',').rep.string.repSep(`,`) <* nl.?).map(Input.apply)

    val result = input.parseAll(s)

    result
      .leftMap(e => s"$e ${e.input.map(s => s.slice(e.failedAtOffset, e.failedAtOffset + 32))}")
      .fold(Tries.fail, identity)
  }

  def part1(input: Input): String > Effects = {
    // println(input.show)
    val result = input.steps.map(hash).sumAll
    s"$result"
  }

  def part2(input: Input): String > Effects = {
    val steps  = input.steps.map(Step.of).groupBy(_.boxId).toVector.sortBy(_._1)
    val boxes  = steps.map(fillBox)
    val result = boxes.map(_.power).sumAll
    s"$result"
  }

  def fillBox(boxId: Int, steps: NonEmptyList[Step]): Box = {
    val lenses = steps
      .foldLeft(Queue.empty[Lens]) {
        case (lenses, Step(label, Op.Remove)) => lenses.filter(_.label != label)
        case (lenses, Step(label, Op.Put(fl))) =>
          val (head, tail) = lenses.span(_.label != label)
          head :+ Lens(label, fl) :++ tail.drop(1)
      }
      .toList
    Box(boxId, lenses)
  }

  case class Box(id: Int, lenses: List[Lens]) {
    def power: Int = {
      // One plus the box number of the lens in question.
      // The slot number of the lens within the box: 1 for the first lens, 2 for the second lens, and so on.
      // The focal length of the lens.
      (lenses.iterator zip Iterator.from(1)).map((l, n) => (1 + id) * n * l.focalLength).sum
    }
  }

  object Box:
    given Show[Box] = b => s"Box ${b.id}: [${b.lenses.mkString_(", ")}]"

  case class Lens(label: String, focalLength: Int)

  object Lens:
    given Show[Lens] = s => s"${s.label} ${s.focalLength}"

  case class Step(label: String, op: Op) {
    val boxId: Int = hash(label)
    override def toString: String = this.show
  }

  object Step {
    def of(s: String): Step = s match {
      case s"$label=$n" => Step(label, op = Op.Put(n.toInt))
      case s"$label-"   => Step(label, op = Op.Remove)
      case s            => sys.error(s"can't parse Step '$s'")
    }

    given Show[Step] = s => s"${s.label}(${s.boxId})"
  }

  enum Op:
    case Remove
    case Put(lens: Int)

  object Op {
    given Show[Op] = {
      case Op.Remove => "-"
      case Op.Put(n) => s"=$n"
    }
  }

  def hash(s: String): Int = {
    s.foldLeft(0) { (h, c) =>
      // Determine the ASCII code for the current character of the string.
      // Increase the current value by the ASCII code you just determined.
      // Set the current value to itself multiplied by 17.
      // Set the current value to the remainder of dividing itself by 256.
      ((h + c.toInt) * 17) % 256
    }
  }
}
