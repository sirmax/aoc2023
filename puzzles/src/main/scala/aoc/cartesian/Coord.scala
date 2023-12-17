package aoc.cartesian

case class Coord(cs: CoordSpace, idx: Int) {
  def r: Int = idx / cs.w
  def c: Int = idx % cs.w
  def rc: (Int, Int) = (r, c)

  def next(dir: Direction): Option[Coord] = dir match {
    case Direction.N => Option.when(idx >= cs.w)(copy(idx = idx - cs.w))
    case Direction.S => Option.when((idx + cs.w) < cs.size)(copy(idx = idx + cs.w))
    case Direction.W => Option.when((idx % cs.w) != 0)(copy(idx = idx - 1))
    case Direction.E => Option.when(((idx + 1) % cs.w) != 0)(copy(idx = idx + 1))
  }

  def manhattanDistance(that: Coord): Int = (this.r - that.r).abs + (this.c - that.c).abs
}
