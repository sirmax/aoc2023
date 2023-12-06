package util

object Math {

  /** Solves a quadratic equation `a*x^2 + b*x + c = 0`.
    * @note `List` is used as a return type only for the UX reasons.
    */
  def solveQuadraticEquation(a: Double, b: Double, c: Double): List[Double] = {
    val d = (b * b) - (4 * a * c)
    d match {
      case 0 =>
        List(-b / 2 * a)
      case d if d > 0 =>
        // (-b +- sqrt(d)) / 2a
        val sd = java.lang.Math.sqrt(d)
        val x1 = (-b - sd) / (2 * a)
        val x2 = (-b + sd) / (2 * a)
        List(x1, x2)
      case _ =>
        List.empty
    }
  }
}
