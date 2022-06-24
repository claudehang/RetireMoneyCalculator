package retcalc

import scala.annotation.tailrec

sealed trait Returns

object Returns {
  // the month starts from 0 and max is 11
  @tailrec
  def monthlyRate(returns: Returns, month: Int): Double = {
    returns match {
      case FixedReturns(annualRate) => annualRate / 12.0
      case VariableReturns(returns) =>
        returns(month % returns.length).monthlyRate
      case OffsetReturns(returns, offset) =>
        monthlyRate(returns, month + offset)
    }
  }
}

case class FixedReturns(annualRate: Double) extends Returns
case class VariableReturn(month: String, monthlyRate: Double)
case class VariableReturns(returns: Vector[VariableReturn]) extends Returns {
  def fromUntil(startMonth: String, endMonth: String): VariableReturns = {
    VariableReturns(
      returns.dropWhile(_.month != startMonth).takeWhile(_.month != endMonth)
    )
  }
}
case class OffsetReturns(returns: Returns, offset: Int) extends Returns
