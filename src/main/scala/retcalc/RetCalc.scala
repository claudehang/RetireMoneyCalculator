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

    (0 until nbOfMonths).foldLeft(initialCapital)((currentCapital, _) =>
      currentCapital * (1 + interestRate) + monthlySavings
    )
  }
}
