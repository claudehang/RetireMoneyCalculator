package retcalc

import scala.annotation.tailrec

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

  def simulatePlan(
      interestRate: Double,
      nbOfMonthsSaving: Int,
      nbOfMonthsRetiring: Int,
      netIncome: Int,
      currentExpense: Int,
      initialCapital: Double
  ): (Double, Double) = {
    val capitalAtRetirement = futureCapital(
      interestRate,
      nbOfMonthsSaving,
      netIncome,
      currentExpense,
      initialCapital
    )
    val capitalAfterDeath = futureCapital(
      interestRate,
      nbOfMonthsRetiring,
      0,
      currentExpense,
      capitalAtRetirement
    )
    (capitalAtRetirement, capitalAfterDeath)
  }

  def nbOfMonthsSaving(
      interestRate: Double,
      nbOfMonthsRetiring: Int,
      netIncome: Int,
      currentExpense: Int,
      initialCapital: Double
  ): Int = {
    @tailrec
    def loop(nbOfMonthsForSaving: Int): Int = {
      val (_, capitalAfterDeath) = simulatePlan(
        interestRate,
        nbOfMonthsForSaving,
        nbOfMonthsRetiring,
        netIncome,
        currentExpense,
        initialCapital
      )
      if (capitalAfterDeath > 0) {
        nbOfMonthsForSaving
      } else {
        loop(nbOfMonthsForSaving + 1)
      }
    }
    if (netIncome < currentExpense) {
      Int.MaxValue
    } else {
      loop(0)
    }
  }
}
