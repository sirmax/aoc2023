import cats.Show
import cats.data.NonEmptyList
import cats.syntax.all.*
import kyo.>
import kyo.apps.App.Effects
import kyo.tries.Tries

import scala.annotation.tailrec
import scala.collection.immutable.Queue

/** [[https://adventofcode.com/2023/day/20]] */
object Day20 extends util.AocApp(2023, 20) {
  case class Input(bCast: Broadcast, modules: List[Module]) {
    val moduleByName: Map[String, Module] = modules.fproductLeft(_.name).toMap

    def renderDot: String = {
      def nodes = {
        def nodeBc = "BC [shape=invtriangle, style=filled, fillcolor=lightblue]"
        def leaves = modules.flatMap(_.output).distinct.map { name =>
          s"$name [shape=doublecircle]"
        }
        def nodes = modules.map { m =>
          val shape = m.mType match {
            case ModuleType.Flip => "square"
            case ModuleType.Conj => "invhouse"
          }
          s"${m.name} [shape=$shape]"
        }
        (nodeBc +: leaves ++: nodes).mkString("\n")
      }
      def edges = {
        def edgeBC = s"BC -> ${bCast.output.mkString(", ")}"
        def edges = modules.map(m => s"${m.name} -> ${m.output.mkString(", ")}")
        (edgeBC +: edges).mkString_("\n")
      }
      s"""digraph {
         $nodes

         edge [dir=forward]
         $edges
      }""".stripMargin
    }
  }

  case class Broadcast(output: List[String])
  case class Module(mType: ModuleType, name: String, output: List[String])

  enum ModuleType:
    case Flip, Conj

  object Input {
    given Show[Input] = Show.fromToString
  }

  def parseInput(s: String): Input > Effects = {
    import cats.parse.{Numbers as N, Rfc5234 as R, Parser as P}

    // broadcaster -> a, b, c
    // %a -> b
    // %b -> c
    // %c -> inv
    // &inv -> a

    val name       = R.alpha.rep.string
    val outputs    = P.string(" -> ") *> name.repSep(P.string(", ")).map(_.toList)
    val moduleType = P.char('%').as(ModuleType.Flip) | P.char('&').as(ModuleType.Conj)
    val module     = (moduleType, name, outputs).mapN(Module.apply)
    val bCast      = P.string("broadcaster") *> outputs.map(Broadcast.apply)

    val input = ((bCast.map(_.asLeft) | module.map(_.asRight)).repSep(R.lf) <* R.lf.?).map { entries =>
      val (List(bCast), modules) = entries.toList.separate
      Input(bCast, modules)
    }

    input.parseAll(s).leftMap(e => throw new IllegalArgumentException(s"Failed to parse\n${e.show}")).merge
  }

  def part1(input: Input): String > Effects = {
    // println(input.show)
    // println(input.renderDot)
    val result = runSequence(input)(1000).log.pulseTotal
    s"$result"
  }

  def part2(input: Input): String > Effects = {
    val result = input.modules.filter(_.output.contains("rx")) match {
      case List(Module(ModuleType.Conj, name, _)) =>
        // Run simulation N times where N is the number of pre-rx conj. inputs.
        // In each run keep only one of those inputs, and count cycles-to-rx.
        // Then cycles-to-rx for the full circuit would be lcm(separate cycles).
        val conjInputs = input.modules.filter(_.output.contains(name))
        val cycles = conjInputs
          .map { m =>
            val toRemove = conjInputs.toSet - m
            val input1   = input.copy(modules = input.modules.filterNot(toRemove))
            runSequence(input1).indexWhere(_.log.rxTriggered).toLong
          }
        cycles.reduce(spire.math.lcm)

      case unsupported => s"can't solve part2 with these inputs for 'rx': ${unsupported}"
    }
    s"$result"
  }

  private def runSequence(input: Input): LazyList[State] = {
    LazyList.iterate(state0(input))(run1(input, _))
  }

  private def state0(input: Input): State = {
    State(
      flip = input.modules.collect { case Module(ModuleType.Flip, name, _) => name -> FlipState() }.toMap,
      conj = input.modules.collect {
        case Module(ModuleType.Conj, name, _) =>
          val inputs = input.modules.filter(_.output.contains(name)).map(_.name)
          name -> ConjState(inputs)
      }.toMap,
      log = SignalLog(),
    )
  }

  private def run1(input: Input, state: State): State = {
    @tailrec def recur(todo: Queue[Signal], s0: State): State = todo match {
      case (signal @ Signal(from, to, pulse)) +: rest =>
        val (s1, signals) = input.moduleByName.get(to) match {
          case Some(Module(ModuleType.Flip, name, output)) =>
            val (moduleState, pulse) = s0.flip(name).run(signal)
            val s1                   = s0.copy(flip = s0.flip.updated(name, moduleState))
            val signals              = (pulse.toList, output).mapN((p, to) => Signal(from = name, to, p))
            (s1, signals)

          case Some(Module(ModuleType.Conj, name, output)) =>
            val (moduleState, pulse) = s0.conj(name).run(signal)
            val s1                   = s0.copy(conj = s0.conj.updated(name, moduleState))
            val signals              = (pulse.toList, output).mapN((p, to) => Signal(from = name, to, p))
            (s1, signals)

          case None =>
            (s0, List.empty)
        }
        recur(rest ++ signals, s1.logSignal(signal))

      case _ => s0
    }

    recur(
      todo = Queue.from(input.bCast.output.map(to => Signal(from = "broadcaster", to, Pulse.L))),
      // initial L->1 accounts for `button -low-> broadcaster` signal
      s0 = state.logSignal(Signal("button", "broadcaster", Pulse.L)),
    )
  }

  case class State(
    flip: Map[String, FlipState],
    conj: Map[String, ConjState],
    log: SignalLog,
  ) {
    def logSignal(s: Signal): State = copy(log = log.log(s))
  }

  case class SignalLog(l: Int = 0, h: Int = 0, rxTriggered: Boolean = false) {
    def pulseTotal: Int = l * h

    def log(s: Signal): SignalLog = {
      val (l1, h1) = s.pulse match {
        case Pulse.L => (l + 1, h)
        case Pulse.H => (l, h + 1)
      }
      SignalLog(l1, h1, rxTriggered = rxTriggered || (s.pulse == Pulse.L && s.to == "rx"))
    }
  }

  /** Flip-flop modules (prefix %) are either on or off; they are initially off.
    * If a flip-flop module receives a high pulse, it is ignored and nothing happens.
    * However, if a flip-flop module receives a low pulse, it flips between on and off.
    * If it was off, it turns on and sends a high pulse.
    * If it was on, it turns off and sends a low pulse.
    */
  case class FlipState(on: Boolean = false) {
    def run(signal: Signal): (FlipState, Option[Pulse]) = signal.pulse match {
      case Pulse.L => (FlipState(!on), (if (on) Pulse.L else Pulse.H).some)
      case Pulse.H => (this, None)
    }
  }

  /** Conjunction modules (prefix &) remember the type of the most recent pulse received
    * ̋from each of their connected input modules; they initially default to remembering
    * a low pulse for each input.
    * When a pulse is received, the conjunction module first updates its memory for that input.
    * Then, if it remembers high pulses for all inputs, it sends a low pulse;
    * otherwise, it sends a high pulse.
    */
  object ConjState {
    def apply(inputs: List[String]): ConjState = ConjState(inputs.tupleRight(Pulse.L).toMap)
  }

  case class ConjState(inputs: Map[String, Pulse]) {
    def run(signal: Signal): (ConjState, Option[Pulse]) = {
      val i1    = inputs.updated(signal.from, signal.pulse)
      val pulse = if (i1.values.forall(_ == Pulse.H)) Pulse.L else Pulse.H
      (ConjState(i1), pulse.some)
    }
  }

  case class Signal(from: String, to: String, pulse: Pulse)

  enum Pulse:
    case L, H
}
