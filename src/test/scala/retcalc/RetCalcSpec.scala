package retcalc

import org.scalactic.{Equality, TolerantNumerics, TypeCheckedTripleEquals}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class RetCalcSpec
    extends AnyWordSpec
    with Matchers
    with TypeCheckedTripleEquals {

  implicit val doubleEquality: Equality[Double] =
    TolerantNumerics.tolerantDoubleEquality(0.0001)

  "RetCalc.futureCapital" should {
    "calculate the amount of savings I will have in n months" in {
      val actual = RetCalc.futureCapital(
        returns = FixedReturns(0.04),
        nbOfMonths = 25 * 12,
        netIncome = 3000,
        currentExpense = 2000,
        initialCapital = 10000
      )

      val expected = 541267.1990
      actual should ===(expected)
    }

    "calculate how much savings will be left after taking pension for n months" in {
      val actual = RetCalc.futureCapital(
        returns = FixedReturns(0.04),
        nbOfMonths = 40 * 12,
        netIncome = 0,
        currentExpense = 2000,
        initialCapital = 541267.1990
      )
      val expected = 309867.53176
      actual should ===(expected)
    }
  }

  val params: RetCalcParams = RetCalcParams(
    nbOfMonthsRetiring = 40 * 12,
    netIncome = 3000,
    currentExpense = 2000,
    initialCapital = 10000
  )

  "RetCalc.simulatePlan" should {
    "calculate the capital at retirement and the capital after death" in {
      val (capitalAtRetirement, capitalAfterDeath) = RetCalc.simulatePlan(
        returns = FixedReturns(0.04),
        nbOfMonthsSaving = 25 * 12,
        params = params
      )
      capitalAtRetirement should ===(541267.1990)
      capitalAfterDeath should ===(309867.5316)
    }
  }

  "RetCalc.nbOfMonthsSaving" should {
    "calculate how long I have to save money before I can retire" in {
      val actual = RetCalc.nbOfMonthsSaving(
        returns = FixedReturns(0.04),
        params = params
      )
      val expected = 23 * 12 + 1
      actual should ===(expected)
    }

    "not loop forever if net income is less than expense" in {
      val actual = RetCalc.nbOfMonthsSaving(
        returns = FixedReturns(0.04),
        params = params.copy(netIncome = 0)
      )
      actual should ===(Int.MaxValue)
    }
  }
}
