package aoc.cartesian

case class Coord(cs: CoordSpace, idx: Int) {
  override def toString: String = s"$xy:$idx"

  def x: Int = idx % cs.w
  def y: Int = idx / cs.w

  def r: Int = y
  def c: Int = x

  def xy: (Int, Int) = (x, y)
  def rc: (Int, Int) = (r, c)

  def next(dir: Direction): Option[Coord] = nextWrap(dir).toOption

  def nextWrap(dir: Direction): Either[Coord, Coord] = dir match {
    case Direction.N =>
      val i = idx - cs.w
      if (i < 0) Left(copy(idx = cs.size + i)) else Right(copy(idx = i))

    case Direction.S =>
      val i = idx + cs.w
      if (i >= cs.size) Left(copy(idx = i - cs.size)) else Right(copy(idx = i))

    case Direction.W =>
      val i = idx - 1
      if (idx % cs.w == 0) Left(copy(idx = i + cs.w)) else Right(copy(idx = i))

    case Direction.E =>
      val i = idx + 1
      if (i % cs.w == 0) Left(copy(idx = i - cs.w)) else Right(copy(idx = i))
  }

  def manhattanDistance(that: Coord): Int = (this.r - that.r).abs + (this.c - that.c).abs
}
