package esp.akka.persistence

import java.util.UUID

import akka.actor.{ActorSystem, Props}
import akka.persistence.{PersistentActor, SnapshotOffer}
import akka.util.Timeout
import esp.Api
import esp.model._
import scala.concurrent.duration._

import scala.concurrent.Await

sealed trait Command

sealed trait Event

case class CreateCommand(user: User) extends Command

case class CreateEvent(uuid: UUID, user: User) extends Event

class UserPersistentActor extends PersistentActor {

  override def persistenceId: String = "user-actor"

  var state = Map[UUID, User]()

  def createHandler(event: CreateEvent): Unit =
    state = state.updated(UUID.randomUUID(), event.user)

  override def receiveCommand: Receive = {
    // TODO: validate command
    case CreateCommand(user) => {
      val newUuid = UUID.randomUUID()
      persistAll(scala.collection.immutable.Seq(CreateEvent(newUuid, user)))(createHandler)
      sender ! newUuid
    }

  }

  override def receiveRecover: Receive = {
    case event: CreateEvent => createHandler(event)
    case SnapshotOffer(_, snapshot: Map[UUID, User]) => state = snapshot
  }
}

trait AkkaPersistenceApi extends Api {

  private val system = ActorSystem("mySystem")

  private val userStore = system.actorOf(Props[UserPersistentActor], "user-actor")

  private implicit val timeout = Timeout(5, SECONDS)

  import akka.pattern.ask

  override def createUser(user: User): UserId = {
    val future = userStore ? CreateCommand(user)
    Await.result(future, Duration(5, SECONDS)).toString
  }

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

object AkkaPersistenceApi