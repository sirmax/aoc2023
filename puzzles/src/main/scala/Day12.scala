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
    // 2501183855078 too low
    val rowsUnfolded = input.rows.map { r =>
      Row(
        List.fill(5)(Gear.`?` :: r.gears).flatten.tail,
        List.fill(5)(r.damages).flatten,
      )
    }
    val result = solve(rowsUnfolded)
    s"$result"
  }

  private def solve(rows: List[Row]) = {
    rows.map { row =>
//      solveRowDumb(row)
      solveRowBetter(row)
    }.sum
  }

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
      nDmg: Int,
      nWrk: Int,
      knDmg: Int,
      knWrk: Int,
      acc: List[Span],
    ) {
      def isValid: Boolean = nWrk >= knWrk && nDmg >= knDmg && spans.headOption.forall(s => allow(s.g))

      def isFinal: Boolean = nWrk == 0 && nDmg == 0

      def next: List[State] =
        if (isFinal || !isValid) List.empty
        else
          (spans, damages) match {
            case ((s @ Span(Gear.`.`, n)) :: spans, damages) =>
              List(State(mul, spans, damages, allowAll, nDmg, nWrk - n, knDmg, knWrk - n, acc :+ s))

            case ((s @ Span(Gear.`#`, n)) :: spans, nd :: damages) =>
              if (n == nd) List(State(mul, spans, damages, allowWorking, nDmg - n, nWrk, knDmg - n, knWrk, acc :+ s))
              else if (n < nd)
                List(State(mul, spans, (nd - n) :: damages, allowDamaged, nDmg - n, nWrk, knDmg - n, knWrk, acc :+ s))
              else List.empty

            case (Span(Gear.`?`, n) :: spans, Nil) =>
              val working =
                if (allow(Gear.`.`)) {
                  List(State(mul, spans, Nil, allowWorking, nDmg, nWrk - n, knDmg, knWrk, acc :+ Span(Gear.`.`, n)))
                } else List.empty
              working

            case (Span(Gear.`?`, n) :: (spans @ Nil), damages) if allow == allowAll =>
              // try to fit all possible combinations of damage spans into n slots
              // collapse each dmg span + 1 adjacent wrk gear into 1
              val dmgTake = damages
              val dmgKeep = Nil
              val toPlace = dmgTake.size
              // +1 compensates for the absent last `.`
              val nPlaces = n + 1 - dmgTake.sum
              val choices = spire.math.choose(nPlaces, toPlace)
              val placedDmg = dmgTake.sum
              val placedWrk = n - placedDmg
//                  println(s"$dmgTake, $n, c($nPlaces, $toPlace) = $choices")
              List(State(mul * choices, spans, dmgKeep, allowWorking, nDmg - placedDmg, nWrk - placedWrk, knDmg, knWrk, acc :+ Span(Gear.`?`, n)))

            case (Span(Gear.`?`, n) :: (spans @ (Span(Gear.`.`, _) :: _)), damages) if allow == allowAll =>
              // try to fit all possible combinations of damage spans into n slots
              val broken = (1 to damages.size)
                .iterator
                .map(damages.splitAt)
                .takeWhile((take, _) => (take.sum + take.size - 1) <= n)
                .map { (dmgTake, dmgKeep) =>
                  // collapse each dmg span + 1 adjacent wrk gear into 1
                  val toPlace = dmgTake.size
                  // +1 compensates for the `.` ahead
                  val nPlaces = n + 1 - dmgTake.sum
                  val choices = spire.math.choose(nPlaces, toPlace)
                  val placedDmg = dmgTake.sum
                  val placedWrk = n - placedDmg
//                  println(s"$dmgTake, $n, c($nPlaces, $toPlace) = $choices")
                  State(mul * choices, spans, dmgKeep, allowWorking, nDmg - placedDmg, nWrk - placedWrk, knDmg, knWrk, acc :+ Span(Gear.`?`, n))
                }
                .toList

              val working1 = State(mul, spans, damages, allow, nDmg, nWrk - n, knDmg, knWrk, acc :+ Span(Gear.`.`, n))
              // Put most placed dmg spans in front
              (working1 :: broken).reverse


            case (Span(Gear.`?`, n1) :: Span(Gear.`#`, n2) :: spans, damages) if allow == allowAll =>
              // try to fit all possible combinations of damage spans into n slots
              // we can't have a definite place of the rightmost dmg span, instead we have to account
              // for all its possible positions on our `?` territory.
//              println(s"${Span(Gear.`?`, n1)} :: ${Span(Gear.`#`, n2)}")
              val broken = (1 to damages.size)
                .iterator
                .map(damages.splitAt)
                .takeWhile((take, _) => (take.sum + take.size - 1) <= n1 + n2)
                .filter((take, _) => take.last >= n2)
                .flatMap { (dmgTakePlus, dmgKeep) =>
                  // collapse each dmg span + 1 adjacent wrk gear into 1
                  val dmgTake = dmgTakePlus.dropRight(1)
                  val dmgLast = dmgTakePlus.last
                  (0 to dmgLast - n2)
                    .filter(dmgTake.sum + dmgTake.size - 1 <= n1 - _)
                    .map { overhead =>
                      val n = n1 - overhead
                      val toPlace = dmgTake.size
                      val nPlaces = n - dmgTake.sum
                      val choices = if (toPlace > 0) spire.math.choose(nPlaces, toPlace) else BigInt(1)
                      val placedDmg = dmgTake.sum
                      val placedWrk = n - placedDmg
                      val dmgPutBack = n2 + overhead
//                      println(s"dmgTake=$dmgTake, n=$n, c($nPlaces, $toPlace) = $choices" )
                      State(mul * choices, Span(Gear.`#`, dmgPutBack) :: spans, dmgPutBack :: dmgKeep, allowDamaged, nDmg - placedDmg, nWrk - placedWrk, knDmg, knWrk, acc :+ Span(Gear.`?`, n))
                    }
                }
                .toList
              // Put most placed dmg spans in front
              broken.reverse

            case (Span(Gear.`?`, n) :: spans, nd :: damages) =>
              val broken =
                if (allow(Gear.`#`)) {
                  if (n == nd)
                    List(
                      State(mul, spans, damages, allowWorking, nDmg - n, nWrk, knDmg, knWrk, acc :+ Span(Gear.`#`, n)),
                    )
                  else if (n < nd)
                    List(
                      State(
                        mul,
                        spans,
                        (nd - n) :: damages,
                        allowDamaged,
                        nDmg - n,
                        nWrk,
                        knDmg,
                        knWrk,
                        acc :+ Span(Gear.`#`, n),
                      ),
                    )
                  else
                    List(
                      State(
                        mul,
                        Span(Gear.`?`, n - nd) :: spans,
                        damages,
                        allowWorking,
                        nDmg - nd,
                        nWrk,
                        knDmg,
                        knWrk,
                        acc :+ Span(Gear.`#`, nd),
                      ),
                    )
                } else List.empty

              val working =
                if (allow(Gear.`.`)) {
                  if (n == 1)
                    List(
                      State(mul, spans, nd :: damages, allowAll, nDmg, nWrk - 1, knDmg, knWrk, acc :+ Span(Gear.`.`, 1)),
                    )
                  else
                    List(
                      State(
                        mul,
                        Span(Gear.`?`, n - 1) :: spans,
                        nd :: damages,
                        allowAll,
                        nDmg,
                        nWrk - 1,
                        knDmg,
                        knWrk,
                        acc :+ Span(Gear.`.`, 1),
                      ),
                    )
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
        val nDmg = row.damages.sum
        val nWrk = row.gears.length - nDmg

        val knownDmg: Int = row.gears.count(_ == Gear.`#`)
        val knownWrk: Int = row.gears.count(_ == Gear.`.`)

        State(
          mul = 1,
          spans,
          row.damages,
          allowAll,
          nDmg = row.damages.sum,
          nWrk = nWrk,
          knDmg = knownDmg,
          knWrk = knownWrk,
          acc = List.empty,
        )
      }

      implicit val show: cats.Show[State] = s =>
        s"[${if (s.isValid) "V" else " "}${if (s.isFinal) "F" else " "} x${s.mul} ${s.acc.show} ${s.spans.show} dmg=${s.damages
            .mkString_(",")}, ${s.allow}, ${s.nDmg}(${s.knDmg}):${s.nWrk}(${s.knWrk})]"
    }

    def spawn(s: State): Iterator[State] = Iterator(s) ++ s.next.iterator/*.filter(_.isValid)*/.flatMap(spawn)

//    val printEach = 1
    val printEach = 1_000_000L

    spawn(State.of(row))
      .zip(Iterator.iterate(0L)(_ + 1L))
      .tapEach((s, i) => if (i % printEach == 0) { println(s"$i ${s.show}") })
      .map(_._1)
      .filter(_.isFinal)
      .map(_.mul)
      .sum
  }

  private def solveRowDumb(row: Row) = {
    val nTotalDamages   = row.damages.sum
    val nUnknowns       = row.gears.count(_ == Gear.`?`)
    val nKnownDamages   = row.gears.count(_ == Gear.`#`)
    val nUnknownDamages = nTotalDamages - nKnownDamages
    val gearsToPlace    = List.fill(nUnknownDamages)(Gear.`#`) ++ List.fill(nUnknowns - nUnknownDamages)(Gear.`.`)

    def place(into: List[Gear], toPlace: List[Gear]) = {
      List.unfold((into, toPlace)) {
        case (Gear.`?` :: restInto, gear :: restToPlace) => Some(gear, (restInto, restToPlace))
        case (gear :: restInto, toPlace)                 => Some(gear, (restInto, toPlace))
        case _                                           => None
      }
    }

    @tailrec def isValid(gears: List[Gear], damages: List[Int]): Boolean = {
      damages match {
        case n :: damages1 =>
          val (group, gears1) = gears.dropWhile(_ == Gear.`.`).span(_ == Gear.`#`)
          if (group.size == n) isValid(gears1, damages1)
          else false
        case Nil =>
          gears.forall(_ == Gear.`.`)
      }
    }

    gearsToPlace.permutations
      .map(place(row.gears, _))
      .count { r =>
        val v = isValid(r, row.damages)
        //            println(s"${r.show} $v ${row.damages}")
        v
      }
  }
}
