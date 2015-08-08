package esp.akka.persistence

import java.util.UUID

import akka.actor.{ActorSystem, Props}
import akka.persistence.{PersistentActor, SnapshotOffer}
import akka.util.Timeout
import esp.Api
import esp.model._
import scala.concurrent.duration._

import scala.concurrent.{Future, Await}

sealed trait Command

sealed trait Event

case class CreateCommand(user: User) extends Command
case class GetCommand(id: String) extends Command

case class CreateEvent(uuid: String, user: User) extends Event
case class GetEvent(id: String) extends Event

class UserPersistentActor extends PersistentActor {

  override def persistenceId: String = "user-actor"

  var state = Map[String, User]()

  def createHandler(event: CreateEvent): Unit =
    state = state.updated(event.uuid, event.user)

  override def receiveCommand: Receive = {
    // TODO: validate command
    case CreateCommand(user) => {
      val newUuid = UUID.randomUUID().toString
      persistAll(scala.collection.immutable.Seq(CreateEvent(newUuid, user)))(createHandler)
      sender ! newUuid
    }

    case GetCommand(id) => {
      persistAll(scala.collection.immutable.Seq(GetEvent(id)))(GetEvent=>())
      sender ! state.get(id)
    }

  }

  override def receiveRecover: Receive = {
    case event: CreateEvent => createHandler(event)
    case SnapshotOffer(_, snapshot: Map[String, User]) => state = snapshot
  }
}

trait AkkaPersistenceApi extends Api {

  private implicit def awaitFuture[T](future : Future[T]) = {
    Await.result(future, Duration(5, SECONDS))
  }

  private val system = ActorSystem("mySystem")

  private val userStore = system.actorOf(Props[UserPersistentActor], "user-actor")

  private implicit val timeout = Timeout(5, SECONDS)

  import akka.pattern.ask

  override def createUser(user: User): UserId = {
    (userStore ? CreateCommand(user)).mapTo[String]
  }

  override def getUser(id: UserId): Option[User] = {
    (userStore ? GetCommand(id)).mapTo[Option[User]]
  }

  override def listAccounts(id: UserId): Seq[AccountNumber] = ???

  override def createAccount(id: UserId): Unit = ???

  override def transferMoney(from: AccountNumber, to: AccountNumber, amount: BigDecimal): Unit = ???

  override def setAccountName(accountNumber: AccountNumber, newName: String): Unit = ???

  override def listTransactionHistory(id: UserId): Seq[Transaction] = ???

  override def depositMoney(accountNumber: AccountNumber, amount: BigDecimal): Unit = ???

  override def changeEmail(id: UserId, email: Option[String]): Unit = ???

  override def getAccount(accountNumber: AccountNumber): Option[Account] = ???
}

object AkkaPersistenceApi