package esp.eventstore

import esp.model.Account

import scalaz._
import Scalaz._

trait AccountValidation {
  def nonEmptyName(name: String) = if (name.nonEmpty) name.successNel else "empty name".failureNel
  def doesntExceed5Accounts(accounts: List[AccountWithUserId]) = if (accounts.length < 5) accounts.successNel else "only 5 accounts allowed".failureNel
}

object AccountValidation extends AccountValidation
