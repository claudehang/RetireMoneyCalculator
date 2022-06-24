package retcalc

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ReturnsSpec extends AnyWordSpec with Matchers {
  "VariableReturns.fromUntil" should {
    "keep only a window of returns" in {
      val variableReturns = VariableReturns(Vector.tabulate(12) { i =>
        val d: Double = (i + 1).toDouble
        VariableReturn(f"2022.$d%02.0f", d)
      })

      variableReturns.fromUntil("2022.03", "2022.05").returns should ===(
        Vector(VariableReturn("2022.03", 3.0), VariableReturn("2022.04", 4.0))
      )

      variableReturns.fromUntil("2022.07", "2022.10").returns should ===(
        Vector(
          VariableReturn("2022.07", 7.0),
          VariableReturn("2022.08", 8.0),
          VariableReturn("2022.09", 9.0)
        )
      )
    }
  }
  "Returns.monthlyRate" should {
    "return fixed rate for FixReturns" in {
      Returns.monthlyRate(FixedReturns(0.04), 2) should ===(0.04 / 12)
      Returns.monthlyRate(FixedReturns(0.04), 8) should ===(0.04 / 12)
    }

    val variableReturns = VariableReturns(
      Vector(VariableReturn("2022.01", 0.01), VariableReturn("2022.02", 0.02))
    )

    "return the returns of the given month for VariableReturns" in {
      Returns.monthlyRate(variableReturns, 0) should ===(0.01)
      Returns.monthlyRate(variableReturns, 1) should ===(0.02)
    }

    "roll over from the first month if the given month > length of VariableReturns" in {
      Returns.monthlyRate(variableReturns, 0) should ===(0.01)
      Returns.monthlyRate(variableReturns, 2) should ===(0.01)
      Returns.monthlyRate(variableReturns, 5) should ===(0.02)
    }

    "return the (n+offset)-th monthly rate for OffsetReturns" in {
      val offsetReturns = OffsetReturns(variableReturns, 1)
      Returns.monthlyRate(offsetReturns, 0) should ===(0.02)
      Returns.monthlyRate(offsetReturns, 2) should ===(0.02)
      Returns.monthlyRate(offsetReturns, 5) should ===(0.01)
    }
  }
}
