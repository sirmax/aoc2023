package aoc.cartesian

enum HVDirection {
  case N, E, S, W

  def inverse: HVDirection = {
    // Assuming the NESW order
    HVDirection.fromOrdinal((this.ordinal + 2) % 4)
  }
}
