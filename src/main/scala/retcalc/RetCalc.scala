package retcalc

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
}
