package aoc.cartesian

enum Direction {
  case N, E, S, W

  def inverse: Direction = {
    // Assuming the NESW order
    Direction.fromOrdinal((this.ordinal + 2) % 4)
  }
}
