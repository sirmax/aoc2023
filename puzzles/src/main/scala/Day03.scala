import kyo.>
import kyo.apps.App.Effects
import kyo.tries.Tries

object Day03 extends util.AocApp(2023, 3) {
  case class Num(value: Int, length: Int)
  opaque type Sym = Char
  opaque type Entry = (Int, Num | Sym)

  case class Input(lines: Vector[Seq[Entry]]) {
    val (syms, nums) = {
      val withCoords = lines.zipWithIndex.flatMap { (entries, r) =>
        entries.map((c, e) => (r, c) -> e)
      }
      withCoords.partitionMap {
        case (coord, x: Sym) => Left(coord -> x)
        case (coord, x: Num) => Right(coord -> x)
      }
    }
  }

  def parseInput(s: String): Input > Tries = {

    import fastparse.*

    implicit object whitespace extends Whitespace {
      def apply(implicit ctx: P[_]): P[Unit] = CharsWhileIn(".", 0)
    }

    def num[$: P] = P(CharsWhileIn("0-9").!).map(x => Num(x.toInt, x.length))
    def sym[$: P] = P(CharIn("#$%&*+/=@") | "-").!.map(_.head: Sym)
    def entry[$: P](nLineStart: Int): P[Entry] = Index.map(_ - nLineStart) ~ P(num | sym)

    def lineStart[$: P] = ((Start.map(_ => 0)) | (Index.map(_ + 1) ~ "\n")) ~ !End
    def line[$: P] = lineStart.flatMap(ls => entry(ls).rep)
    def input[$: P] = line.rep.map(x => Input(lines = x.toVector))

    val result = parse(s, input)
    Tries.catching(result.get.value)
  }

  def part1(input: Input): String > Effects = {
    val symsMap = input.syms.toMap

    val result = input.nums
      .flatMap { (coord, num) =>
        val hasSym = coordsAround(coord, num).exists(symsMap.keySet)
        Option.when(hasSym)(num)
      }
      .map(_.value)
      .sum
    s"$result"
  }

  def part2(input: Input): String > Effects = {
    val gearCoords = input.syms.collect { case (coord, '*') => coord }.toSet

    val result = input.nums
      .flatMap((coord, num) => coordsAround(coord, num).filter(gearCoords).map(_ -> num))
      .groupMap(_._1)(_._2)
      .collect { case (_, parts) if parts.length == 2 => parts.map(_.value).product.toLong }
      .sum
    s"$result"
  }

  private def coordsAround(coord: (Int, Int), num: Num) = {
    val (r, c) = coord
    val above  = ((c - 1) to (c + num.length)).map((r - 1, _))
    val mid    = Vector((r, c - 1), (r, c + num.length))
    val below  = ((c - 1) to (c + num.length)).map((r + 1, _))
    above ++ mid ++ below
  }

}
