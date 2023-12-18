import cats.data.NonEmptyList
import cats.syntax.all.*
import kyo.>
import kyo.apps.App.Effects
import kyo.tries.Tries

object Day08 extends util.AocApp(2023, 8) {
  case class Input(turns: Turns, nodes: Nodes)

  case class Turns(seq: Vector[Turn])

  enum Turn:
    case L, R

  case class Nodes(seq: NonEmptyList[Node]) {
    val lookup: Map[String, Node] = seq.fproductLeft(_.name).toList.toMap
  }

  case class Node(name: String, l: String, r: String) {
    def next(turn: Turn): String = turn match
      case Turn.L => l
      case Turn.R => r
  }

  def parseInput(s: String): Input > Effects = {
    import cats.parse.{Numbers as N, Rfc5234 as R, Parser as P}
    import cats.syntax.all.*

    val sps   = R.sp.rep.void
    val long  = N.digits.map(_.toLong).surroundedBy(R.sp.rep0)
    val longs = long.rep
    val nl    = P.char('\n')

    val turn  = P.charIn(Turn.values.map(_.toString.charAt(0))).map(c => Turn.valueOf(c.toString))
    val turns = turn.rep.map(x => Turns(x.toList.toVector))
    val name  = P.string((R.alpha | R.digit).rep)
    val node = (name, P.string(" = ("), name, P.string(", "), name, P.string(")"))
      .mapN((name, _, l, _, r, _) => Node(name, l, r))
    val nodes = node.repSep(nl).map(Nodes.apply)

    val input = ((turns <* nl.rep) ~ (nodes <* nl.?)).map(Input.apply)

    val result = input.parseAll(s)
    result
      .leftMap(e => s"$e ${e.input.map(s => s.slice(e.failedAtOffset, e.failedAtOffset + 32))}")
      .fold(Tries.fail, identity)
  }

  private case class RunState(nSteps: Int, atNode: String)

  private def run(input: Input, from: String, end: String => Boolean) = {
    def loopTurns: Iterator[Turn] = input.turns.seq.iterator ++ loopTurns

    loopTurns
      .scanLeft(RunState(0, from)) { (s, turn) =>
        s.copy(s.nSteps + 1, input.nodes.lookup(s.atNode).next(turn))
      }
      .drop(1) // skip the start
      .find(s => end(s.atNode))
  }

  def part1(input: Input): String > Effects = {
    val result = run(input, from = "AAA", end = _ == "ZZZ")
      .map(_.nSteps)
      .get
    s"$result"
  }

  def part2(input: Input): String > Effects = {
    val result = input.nodes.seq.toList
      .mapFilter(_.name.some.filter(_.endsWith("A")))
      .map { aNode =>
        val fromA = run(input, aNode, _.endsWith("Z")).get
        val fromZ = run(input, fromA.atNode, _.endsWith("Z")).get

        assert(
          fromA.nSteps == fromZ.nSteps && fromA.atNode == fromZ.atNode,
          s"More complicated cases aren't yet supported: $fromA, $fromZ",
        )
        fromZ.nSteps.toLong
      }
      .reduce(spire.math.lcm)

    s"$result"
  }
}
