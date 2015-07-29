package esp

import java.util.UUID

package object model {
  type UserId = String
  type AccountNumber = String
}

package model {

case class User(firstName: String, lastName: String, phone: Option[String], email: Option[String])

case class Account(number: AccountNumber, name: String, balance: BigDecimal)

case class Transaction(from: Option[AccountNumber], to: AccountNumber, amount: BigDecimal)

object Bank {
  val Account = esp.model.Account(UUID.randomUUID().toString, "main-bank-account", 1024)

  def transferFee: BigDecimal => BigDecimal = _ * 0.03
}

}



