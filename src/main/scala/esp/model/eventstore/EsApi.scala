package esp.model.eventstore

import esp.Api
import esp.model._

trait EsApi extends Api {

  override def createUser(user: User): UserId = UserFunctions.ucCreateUser(user)
  override def getUser(id: UserId): Option[User] = UserFunctions.ucGetUser(id)
  override def changeEmail(id: UserId, email: Option[String]): Unit = ???

  override def createAccount(id: UserId): Unit = ???
  override def setAccountName(accountNumber: AccountNumber, newName: String): Unit = ???
  override def getAccount(accountNumber: AccountNumber): Option[Account] = ???
  override def listAccounts(id: UserId): Seq[AccountNumber] = ???

  override def depositMoney(accountNumber: AccountNumber, amount: BigDecimal): Unit = ???
  override def transferMoney(from: AccountNumber, to: AccountNumber, amount: BigDecimal): Unit = ???

  override def listTransactionHistory(id: UserId): Seq[Transaction] = ???

}

object EsApi extends EsApi
