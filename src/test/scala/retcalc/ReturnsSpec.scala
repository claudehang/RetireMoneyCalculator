package retcalc

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ReturnsSpec extends AnyWordSpec with Matchers {
  "VariableReturns.fromUntil()" should {
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
}
