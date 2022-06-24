package retcalc

import scala.annotation.tailrec

case class RetCalcParams(
    nbOfMonthsRetiring: Int,
    netIncome: Int,
    currentExpense: Int,
    initialCapital: Double
)

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
      params: RetCalcParams
  ): (Double, Double) = {
    import params._
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
      params: RetCalcParams
  ): Int = {
    import params._
    @tailrec
    def loop(nbOfMonthsForSaving: Int): Int = {
      val (_, capitalAfterDeath) = simulatePlan(
        returns,
        nbOfMonthsForSaving,
        params
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
