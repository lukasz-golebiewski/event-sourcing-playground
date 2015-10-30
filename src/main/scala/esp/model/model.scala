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
    var Account = esp.model.Account(UUID.randomUUID().toString, "main-bank-account", 1024)
    def transferFee: BigDecimal => BigDecimal = _ * 0.03
  }

}

trait Api {

  import esp.model._

  def createUser(user: User): UserId

  def listTransactionHistory(id: UserId): Seq[Transaction]

  def getUser(id: UserId): Option[User]

  def listAccounts(id: UserId): Seq[AccountNumber]

  def changeEmail(id: UserId, email: Option[String]): Unit

  def setAccountName(accountNumber: AccountNumber, newName: String): Unit

  def getAccount(accountNumber: AccountNumber): Option[Account]

  def createAccount(id: UserId): Unit

  def depositMoney(accountNumber: AccountNumber, amount: BigDecimal): Unit

  def transferMoney(from: AccountNumber, to: AccountNumber, amount: BigDecimal): Unit

}
