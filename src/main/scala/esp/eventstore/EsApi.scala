package esp.eventstore

import esp.Api
import esp.model._

trait EsApi extends Api {

  override def createUser(user: User): UserId = UserFunctions.createUser(user)
  override def getUser(id: UserId): Option[User] = UserFunctions.getUser(id)
  override def changeEmail(id: UserId, email: Option[String]): Unit = UserFunctions.changeEmail(id)(email)

  override def createAccount(id: UserId): Unit = AccountFunctions.createAccount(id)
  override def setAccountName(accountNumber: AccountNumber, newName: String): Unit = AccountFunctions.setAccountName(accountNumber)(newName)
  override def getAccount(accountNumber: AccountNumber): Option[Account] = AccountFunctions.getAccount(accountNumber)
  override def listAccounts(id: UserId): Seq[AccountNumber] = AccountFunctions.listAccount(id)

  override def depositMoney(accountNumber: AccountNumber, amount: BigDecimal): Unit = AccountFunctions.depositMoney(accountNumber)(amount)
  override def transferMoney(from: AccountNumber, to: AccountNumber, amount: BigDecimal): Unit = AccountFunctions.transferMoney(from)(to)(amount)

  override def listTransactionHistory(id: UserId): Seq[Transaction] = AccountFunctions.listTransaction(id)

}

object EsApi extends EsApi
