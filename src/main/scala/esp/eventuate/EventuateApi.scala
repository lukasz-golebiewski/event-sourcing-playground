package esp.eventuate

import akka.actor.ActorRef
import com.rbmhtechnology.eventuate.EventsourcedActor
import esp.Api
import esp.model._

class UserActor(override val id: String,override val eventLog: ActorRef) extends EventsourcedActor {

  override def onEvent: Receive = ???

  override def onCommand: Receive = ???
}

trait EventuateApi extends Api {
  override def createUser(user: User): UserId = ???

  override def listAccounts(id: UserId): Seq[AccountNumber] = ???

  override def createAccount(id: UserId): Unit = ???

  override def getUser(id: UserId): Option[User] = ???

  override def transferMoney(from: AccountNumber, to: AccountNumber, amount: BigDecimal): Unit = ???

  override def setAccountName(accountNumber: AccountNumber, newName: String): Unit = ???

  override def listTransactionHistory(id: UserId): Seq[Transaction] = ???

  override def depositMoney(accountNumber: AccountNumber, amount: BigDecimal): Unit = ???

  override def changeEmail(id: UserId, email: Option[String]): Unit = ???

  override def getAccount(accountNumber: AccountNumber): Option[Account] = ???
}
