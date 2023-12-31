import cats.data.NonEmptyList
import cats.syntax.all.*
import kyo.>
import kyo.apps.App.Effects
import kyo.tries.Tries

import scala.annotation.tailrec

object Day12 extends util.AocApp(2023, 12) {
  case class Input(rows: List[Row])
  case class Row(gears: List[Gear], damages: List[Int])

  enum Gear:
    case `.`, `#`, `?`

  private given cats.Show[List[Gear]] = _.mkString
  private given cats.Show[Set[Gear]] = s => s"(${Gear.values.toList.filter(s).mkString})"

  def parseInput(s: String): Input > Effects = {
    import cats.parse.{Numbers as N, Rfc5234 as R, Parser as P}
    import cats.syntax.all.*

    val nl  = P.char('\n')
    val int = R.digit.rep.string.map(_.toInt)

    val gear    = P.charIn(Gear.values.map(_.toString.head)).string.map(Gear.valueOf)
    val gears   = gear.rep.map(_.toList)
    val damages = int.repSep(P.char(',').string).map(_.toList.map(_.toInt))
    val row     = (gears, R.sp, damages).mapN((gears, _, damages) => Row(gears, damages))
    val rows    = row.repSep(nl).map(_.toList) <* nl.?

    val input = rows.map(Input.apply)

    val result = input.parseAll(s)

    result
      .leftMap(e => s"$e ${e.input.map(s => s.slice(e.failedAtOffset, e.failedAtOffset + 32))}")
      .fold(Tries.fail, identity)
  }

  def part1(input: Input): String > Effects = {
    val result = solve(input.rows)
    s"$result"
  }

  def part2(input: Input): String > Effects = {
    val rowsUnfolded = input.rows.map { r =>
      Row(
        List.fill(5)(Gear.`?` :: r.gears).flatten.tail,
        List.fill(5)(r.damages).flatten,
      )
    }
    val result = solve(rowsUnfolded)
    s"$result"
  }

  private def solve(rows: List[Row]) = rows.map(solveRowBetter).sum

  private def solveRowBetter(row: Row): BigInt = {
    val allowAll     = Set(Gear.`.`, Gear.`#`, Gear.`?`)
    val allowWorking = Set(Gear.`.`, Gear.`?`)
    val allowDamaged = Set(Gear.`#`, Gear.`?`)

    case class Span(g: Gear, n: Int)
    object Span {
//      given cats.Show[Span] = s => s"${s.g}${s.n}"
      given cats.Show[Span] = s => s"${s.g}" * s.n
      given cats.Show[List[Span]] = _.mkString_("")
    }

    case class State(
      mul: BigInt,
      spans: List[Span],
      damages: List[Int],
      allow: Set[Gear],
      acc: List[Span],
    ) {
      def isValid: Boolean = spans.headOption
        .forall(s => allow(s.g)) && (damages.iterator.sum <= spans.iterator.filter(_.g != Gear.`.`).map(_.n).sum)

      def isFinal: Boolean = spans.isEmpty && damages.isEmpty

      def next: List[State] =
        if (isFinal || !isValid) List.empty
        else
          (spans, damages) match {
            case ((s @ Span(Gear.`.`, n)) :: spans, damages) =>
//              println(s"case ((s @ Span(Gear.`.`, n)) :: spans, damages) =>")
              List(State(mul, spans, damages, allowAll, acc :+ s))

            case ((s @ Span(Gear.`#`, n)) :: spans, nd :: damages) =>
//              println(s"case ((s @ Span(Gear.`#`, n)) :: spans, nd :: damages) =>")
              if (n == nd) List(State(mul, spans, damages, allowWorking, acc :+ s))
              else if (n < nd) List(State(mul, spans, (nd - n) :: damages, allowDamaged, acc :+ s))
              else List.empty

            case (Span(Gear.`?`, n) :: spans, Nil) =>
//              println(s"case (Span(Gear.`?`, n) :: spans, Nil) =>")
              val working =
                if (allow(Gear.`.`)) List(State(mul, spans, Nil, allowWorking, acc :+ Span(Gear.`.`, n)))
                else List.empty
              working

            case (Span(Gear.`?`, n) :: (spans @ Nil), damages) if allow == allowAll =>
              // println(s"case (Span(Gear.`?`, n) :: (spans @ Nil), damages) if allow == allowAll =>")
              // try to fit all possible combinations of damage spans into n slots
              // collapse each dmg span + 1 adjacent wrk gear into 1
              if (damages.sum + damages.size - 1 <= n) {
                val dmgTake = damages
                val dmgKeep = Nil
                val toPlace = dmgTake.size
                // +1 compensates for the absent last `.`
                val nPlaces = n + 1 - dmgTake.sum
                //                            println(s"$dmgTake, $n, c($nPlaces, $toPlace) = ???")
                val choices = spire.math.choose(nPlaces, toPlace)
                // println(s"$dmgTake, $n, c($nPlaces, $toPlace) = $choices")
                List(State(mul * choices, spans, dmgKeep, allowWorking, acc :+ Span(Gear.`?`, n)))
              } else List.empty

            case (Span(Gear.`?`, n) :: (spans @ (Span(Gear.`.`, _) :: _)), damages) if allow == allowAll =>
//              println(s"case (Span(Gear.`?`, n) :: (spans @ (Span(Gear.`.`, _) :: _)), damages) if allow == allowAll =>")
              // try to fit all possible combinations of damage spans into n slots
              val broken = (1 to damages.size).iterator
                .map(damages.splitAt)
                .takeWhile((take, _) => (take.sum + take.size - 1) <= n)
                .map { (dmgTake, dmgKeep) =>
                  // collapse each dmg span + 1 adjacent wrk gear into 1
                  val toPlace = dmgTake.size
                  // +1 compensates for the `.` ahead
                  val nPlaces = n + 1 - dmgTake.sum
                  val choices = spire.math.choose(nPlaces, toPlace)
//                  println(s"$dmgTake, $n, c($nPlaces, $toPlace) = $choices")
                  State(mul * choices, spans, dmgKeep, allowWorking, acc :+ Span(Gear.`?`, n))
                }
                .toList

              val working1 = State(mul, spans, damages, allow, acc :+ Span(Gear.`.`, n))
              // Put most placed dmg spans in front
              (working1 :: broken).reverse

            case (Span(Gear.`?`, n1) :: Span(Gear.`#`, n2) :: spans, damages) if allow == allowAll =>
//              println(s"case (Span(Gear.`?`, n1) :: Span(Gear.`#`, n2) :: spans, damages) if allow == allowAll =>")
              // try to fit all possible combinations of damage spans into n slots
              // we can't have a definite place of the rightmost dmg span, instead we have to account
              // for all its possible positions on our `?` territory.
              // println(s"${Span(Gear.`?`, n1)} :: ${Span(Gear.`#`, n2)}")
              val broken = (1 to damages.size).iterator
                .map(damages.splitAt)
//                .takeWhile((take, _) => (take.sum + take.size - 1) <= n1 + n2)
                .map((take, keep) => (take.dropRight(1), take.last, keep))
                .filter((_, dmgLast, _) => dmgLast >= n2)
                .flatMap { (take, dmgLast, keep) =>
                  (0 to math.min(n1, dmgLast - n2)).map((take, _, dmgLast, keep))
                }
                // .tapEach(println)
                .filter((take, overhead, _, _) => math.max(0, take.sum + take.size) <= n1 - overhead)
                .map { (dmgTake, overhead, dmgLast, dmgKeep) =>
                  // collapse each dmg span + 1 adjacent wrk gear into 1
                  val n          = n1 - overhead
                  val toPlace    = dmgTake.size
                  val nPlaces    = n - dmgTake.sum
                  val choices0   = spire.math.choose(nPlaces, toPlace)
                  val choices    = if (choices0 > 0) choices0 else BigInt(1)
                  val dmgPutBack = n2 + overhead

                  // This is just for debugging purposes to get a more precise picture on the printout.
                  val spanToAccumulate = Span(if (toPlace == 0) Gear.`.` else Gear.`?`, n)

                  // println(
                  //  s"dmgTake=$dmgTake, n=$n, n2=$n2, c($nPlaces, $toPlace) = $choices, overhead=$overhead. dmgPutBack=$dmgPutBack",
                  // )
                  State(
                    mul * choices,
                    Span(Gear.`#`, dmgPutBack) :: spans,
                    dmgLast :: dmgKeep,
                    allowDamaged,
                    acc :+ spanToAccumulate,
                  )
                }
                .toList
              // Put most placed dmg spans in front
              broken.reverse

            case (Span(Gear.`?`, n) :: spans, nd :: damages) =>
//              println(s"case (Span(Gear.`?`, n) :: spans, nd :: damages) =>")
              val broken =
                if (allow(Gear.`#`)) {
                  if (n == nd)
                    List(State(mul, spans, damages, allowWorking, acc :+ Span(Gear.`#`, n)))
                  else if (n < nd)
                    List(State(mul, spans, (nd - n) :: damages, allowDamaged, acc :+ Span(Gear.`#`, n)))
                  else
                    List(State(mul, Span(Gear.`?`, n - nd) :: spans, damages, allowWorking, acc :+ Span(Gear.`#`, nd)))
                } else List.empty

              val working =
                if (allow(Gear.`.`)) {
                  if (n == 1)
                    List(State(mul, spans, nd :: damages, allowAll, acc :+ Span(Gear.`.`, 1)))
                  else
                    List(State(mul, Span(Gear.`?`, n - 1) :: spans, nd :: damages, allowAll, acc :+ Span(Gear.`.`, 1)))
                } else List.empty

              broken ++ working

            case (_, Nil) => List.empty
            case (Nil, _) => List.empty
          }
    }

    object State {
      def of(row: Row): State = {
        val spans = row.gears.foldRight(List.empty[Span]) {
          case (gear, Span(g, n) :: spans) if gear == g => Span(g, n + 1) :: spans
          case (gear, spans)                            => Span(gear, 1) :: spans
        }

        State(mul = 1, spans, row.damages, allowAll, acc = List.empty)
      }

      implicit val show: cats.Show[State] = s => {
        val v    = if (s.isValid) "V" else " "
        val f    = if (s.isFinal) "F" else " "
        val dmgs = s.damages.mkString_(",")
        s"[$v$f x${s.mul} ${s.acc.show}◀︎${s.spans.show} $dmgs, ${s.allow.show}]"
      }
    }

    def spawn(s: State): Iterator[State] = {
      val next = s.next
      // .filter(_.isValid)

      // println(s"${s.show}${next.mkString_("\nvvvvv\n", "\n", "")}")
      Iterator(s) ++ next.iterator.filter(_.isValid).flatMap(spawn)
    }

    // val printEach = 1
    val printEach = 1_000_000L

    spawn(State.of(row))
      .zip(Iterator.iterate(0L)(_ + 1L))
      .tapEach((s, i) => if (i % printEach == 0 || s.mul == 0) { println(s"$i ${s.show}") })
      .map(_._1)
      .takeWhile(s => s.mul > 0)
      .filter(_.isFinal)
      .map(_.mul)
      .sum
  }
}
