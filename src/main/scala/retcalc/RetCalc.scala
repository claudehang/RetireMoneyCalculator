package retcalc

import scala.annotation.unused

object RetCalc {
  def futureCapital(
      interestRate: Double,
      nbOfMonths: Int,
      netIncome: Int,
      currentExpense: Int,
      initialCapital: Double
  ): Double = {
    val monthlySavings = netIncome - currentExpense

    def nextCapital(currentCapital: Double, @unused nbOfMonths: Int): Double =
      currentCapital * (1 + interestRate) + monthlySavings

    (0 until nbOfMonths).foldLeft(initialCapital)(nextCapital)
  }
}
