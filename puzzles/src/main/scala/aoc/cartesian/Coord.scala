package aoc.cartesian

case class Coord(cs: CoordSpace, idx: Int) {
  def r: Int = idx / cs.w
  def c: Int = idx % cs.w
  def rc: (Int, Int) = (r, c)

  def next(dir: HVDirection): Option[Coord] = dir match {
    case HVDirection.N => Option.when(idx >= cs.w)(copy(idx = idx - cs.w))
    case HVDirection.S => Option.when((idx + cs.w) < cs.size)(copy(idx = idx + cs.w))
    case HVDirection.W => Option.when((idx % cs.w) != 0)(copy(idx = idx - 1))
    case HVDirection.E => Option.when(((idx + 1) % cs.w) != 0)(copy(idx = idx + 1))
  }
}
