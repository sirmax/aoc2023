package aoc.cartesian

enum Direction {
  case N, E, S, W

  def turn(rotation: Rotation): Direction = {
    // Assuming the NESW order
    val diff = rotation match
      case Rotation.CW  => 1
      case Rotation.CCW => 3
    Direction.fromOrdinal((this.ordinal + diff) % 4)
  }

  def inverse: Direction = {
    // Assuming the NESW order
    Direction.fromOrdinal((this.ordinal + 2) % 4)
  }
}
