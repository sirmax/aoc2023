package aoc.cartesian

import cats.syntax.all.*

import scala.collection.immutable.BitSet

case class CoordSpace(w: Int, h: Int) {
  val size: Int = w * h

  def coord(i: Int): Coord = Coord(this, i)
  def coord(x: Int, y: Int): Coord = Coord(this, w * y + x)

  def rowNums: Range = 0 until h
  def colNums: Range = 0 until w

  def coords: Iterator[Coord] = Iterator.range(0, size).map(coord)
  def rowCoords(r: Int): Iterator[Coord] = colNums.iterator.map(coord(_, r))
  def colCoords(c: Int): Iterator[Coord] = rowNums.iterator.map(coord(c, _))

  def render(empty: String, masks: Seq[(String, BitSet)]): String = {
    def charAt(idx: Int) = masks.collectFirstSome((ch, mask) => Option.when(mask(idx))(ch)).getOrElse(empty)
    Iterator.range(0, size).map(charAt).grouped(w).map(_.mkString).mkString("\n")
  }
}
