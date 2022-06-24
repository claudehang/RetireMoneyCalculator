package retcalc

import scala.annotation.tailrec

object RetCalc {
  def futureCapital(
      returns: Returns,
      nbOfMonths: Int,
      netIncome: Int,
      currentExpense: Int,
      initialCapital: Double
  ): Double = {
    val monthlySavings = netIncome - currentExpense

    (0 until nbOfMonths).foldLeft(initialCapital) {
      case (currentCapital, month) =>
        currentCapital * (1 + Returns.monthlyRate(
          returns,
          month
        )) + monthlySavings
    }
  }

  def simulatePlan(
      returns: Returns,
      nbOfMonthsSaving: Int,
      nbOfMonthsRetiring: Int,
      netIncome: Int,
      currentExpense: Int,
      initialCapital: Double
  ): (Double, Double) = {
    val capitalAtRetirement = futureCapital(
      returns: Returns,
      nbOfMonthsSaving,
      netIncome,
      currentExpense,
      initialCapital
    )
    val capitalAfterDeath = futureCapital(
      returns: Returns,
      nbOfMonthsRetiring,
      0,
      currentExpense,
      capitalAtRetirement
    )
    (capitalAtRetirement, capitalAfterDeath)
  }

  def nbOfMonthsSaving(
      returns: Returns,
      nbOfMonthsRetiring: Int,
      netIncome: Int,
      currentExpense: Int,
      initialCapital: Double
  ): Int = {
    @tailrec
    def loop(nbOfMonthsForSaving: Int): Int = {
      val (_, capitalAfterDeath) = simulatePlan(
        returns,
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
