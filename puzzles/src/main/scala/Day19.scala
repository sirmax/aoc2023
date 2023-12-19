import cats.Show
import cats.data.NonEmptyList
import cats.syntax.all.*
import kyo.>
import kyo.apps.App.Effects
import kyo.tries.Tries

import scala.annotation.tailrec
import scala.collection.immutable.BitSet

object Day19 extends util.AocApp(2023, 19) {

  case class Input(workflows: List[Workflow], parts: List[Part]) {
    val wfByName: Map[String, Workflow] = workflows.fproductLeft(_.name).toMap
  }

  object Input {
    given Show[Input] = i => s"${i.workflows.mkString("\n")}\n\n${i.parts.mkString("\n")}"
  }

  case class Workflow(name: String, rules: List[Rule], default: Action) {
    override def toString: String = s"$name { ${rules.mkString(", ")}, $default }"
  }

  case class Rule(xmas: XMAS, cmp: Cmp, action: Action) {
    def run(p: Part): Option[Action] = {
      Option.when(cmp.satisfies(p.get(xmas)))(action)
    }
    override def toString: String = s"$xmas$cmp:$action"
  }

  enum Cmp {
    case Lt(n: Int)
    case Gt(n: Int)
    case Eq(n: Int)

    def satisfies(x: Int): Boolean = this match {
      case Cmp.Lt(n) => x < n
      case Cmp.Gt(n) => x > n
      case Cmp.Eq(n) => x == n
    }

    override def toString: String = this match {
      case Cmp.Lt(n) => s"<$n"
      case Cmp.Gt(n) => s">$n"
      case Cmp.Eq(n) => s"=$n"
    }
  }

  enum Action:
    case Accept, Reject
    case Next(wfName: String)

  case class Part(x: Int, m: Int, a: Int, s: Int) {
    def rating: Int = x + m + a + s

    def get(xmas: XMAS): Int = xmas match {
      case XMAS.x => x
      case XMAS.m => m
      case XMAS.a => a
      case XMAS.s => s
    }

    override def toString: String = s"{x=$x, m=$m, a=$a, s=$s}"
  }

  enum XMAS:
    case x, m, a, s

  def parseInput(s: String): Input > Effects = {
    import cats.parse.{Numbers as N, Rfc5234 as R, Parser as P}

    // px{a<2006:qkq,m>2090:A,rfg}
    // pv{a>1716:R,A}
    // lnx{m>1548:A,A}
    // rfg{s<537:gd,x>2440:R,A}
    // qs{s>3448:A,lnx}
    // qkq{x<1416:A,crn}
    // crn{x>2662:A,R}
    // in{s<1351:px,qqz}
    // qqz{s>2770:qs,m<1801:hdj,R}
    // gd{a>3333:R,R}
    // hdj{m>838:A,pv}
    //
    // {x=787,m=2655,a=1222,s=2876}
    // {x=1679,m=44,a=2067,s=496}
    // {x=2036,m=264,a=79,s=2244}
    // {x=2461,m=1339,a=466,s=291}
    // {x=2127,m=1623,a=2188,s=1013}

    val int    = R.digit.rep.string.map(_.toInt)
    val wfName = R.alpha.rep.string
    val xmas   = P.stringIn(XMAS.values.map(_.toString)).map(XMAS.valueOf)

    val cmp =
      (P.char('<') *> int).map(Cmp.Lt.apply) |
        (P.char('>') *> int).map(Cmp.Gt.apply) |
        (P.char('=') *> int).map(Cmp.Eq.apply)

    val action = P.char('A').as(Action.Accept) | P.char('R').as(Action.Reject) | wfName.map(Action.Next.apply)

    val rule = (xmas, cmp <* P.char(':'), action).mapN(Rule.apply).backtrack

    val wf = {
      val `,`   = P.char(',')
      val rules = (rule <* `,`).rep.map(_.toList)
      (wfName <* P.char('{'), rules, action <* P.char('}')).mapN(Workflow.apply)
    }

    val part = P.until(R.lf).string.map { case s"{x=$x,m=$m,a=$a,s=$s}" => Part(x.toInt, m.toInt, a.toInt, s.toInt) }

    val input = (wf.repSep(R.lf) <* R.lf <* R.lf, part.repSep(R.lf))
      .mapN { (wfs, parts) => Input(wfs.toList, parts.toList) }
      <* R.lf.?

    input.parseAll(s).leftMap(e => throw new IllegalArgumentException(s"Failed to parse\n${e.show}")).merge
  }

  def part1(input: Input): String > Effects = {
    // println(input.show)

    def keep(wfs: Map[String, Workflow], part: Part): Boolean = {
      wfs("in").tailRecM[cats.Id, Boolean] { wf =>
        wf.rules.collectFirstSome(_.run(part)).getOrElse(wf.default) match {
          case Action.Accept       => true.asRight
          case Action.Reject       => false.asRight
          case Action.Next(wfName) => wfs(wfName).asLeft
        }
      }
    }

    val result = input.parts
      .filter(keep(input.wfByName, _))
      .map(_.rating)
      .sum
    s"$result"
  }

  def part2(input: Input): String > Effects = {
    val max = 4000
    def mask(range: Range) = BitSet.fromSpecific(range)

    case class Masks(masks: Map[XMAS, BitSet] = XMAS.values.toList.tupleRight(mask(1 to max)).toMap) {
      def size: Long = masks.values.map(_.size.toLong).product

      /** (not matched, matched) */
      def split(xmas: XMAS, cmp: Cmp): (Option[Masks], Option[Masks]) = {
        val m = masks(xmas)
        val matchMask = cmp match {
          case Cmp.Lt(n) => mask(1 until n)
          case Cmp.Gt(n) => mask((n + 1) to max)
          case Cmp.Eq(n) => mask(n to n)
        }
        def next(m: BitSet) = m.some.filter(_.nonEmpty).map(masks.updated(xmas, _)).map(Masks.apply)
        (next(m -- matchMask), next(m.intersect(matchMask)))
      }
    }

    def runWf(wf: Workflow, masks: Masks): List[(Masks, Action)] = {
      val (remainingMasks, matched) = wf.rules.mapAccumulate(masks.some) {
        case (Some(masks), rule) =>
          val (notMatched, matched) = masks.split(rule.xmas, rule.cmp)
          (notMatched, matched.tupleRight(rule.action))
        case (None, _) => (None, None)
      }
      (remainingMasks.tupleRight(wf.default) :: matched).flatten
    }

    @tailrec def recur(
      todo: List[(Masks, Action)] = List(Masks() -> Action.Next("in")),
      accepted: List[Masks] = List.empty,
    ): List[Masks] = {
      todo match {
        case (masks, Action.Next(wfName)) :: rest =>
          val addTodo = runWf(input.wfByName(wfName), masks)
          recur(addTodo ++: rest, accepted)
        case (masks, Action.Accept) :: rest => recur(rest, masks :: accepted)
        case (masks, Action.Reject) :: rest => recur(rest, accepted)
        case Nil                            => accepted
      }
    }

    val result = recur()
      .map(_.size)
      .sum
    s"$result"
  }
}
