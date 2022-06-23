package retcalc

sealed trait Returns

case class VariableReturn(month: String, monthlyRate: Double)
case class VariableReturns(returns: Vector[VariableReturn]) extends Returns {
  def fromUntil(startMonth: String, endMonth: String): VariableReturns = {
    VariableReturns(
      returns.dropWhile(_.month != startMonth).takeWhile(_.month != endMonth)
    )
  }
}
