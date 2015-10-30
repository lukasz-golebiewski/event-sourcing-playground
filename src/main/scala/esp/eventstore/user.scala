package esp.eventstore

import java.util.UUID

import esp.model.{AccountNumber, Account, UserId, User}

import scalaz._
import Scalaz._

trait UserBusinessLogic {

  //TODO: how return validation messages?
  def ucChangeEmail: ChangeEmail => List[Event] =
    cmd =>
      (for {
        email <- cmd.email.toSuccess("empty email")
        validated <- UserValidation.validate(email)
      } yield EmailChanged(cmd.userId, validated)).toList

  def ucCreateUser: CreateUser => UserId => AccountNumber => List[Event] =
    cmd => userId => accountNumber =>
      List(UserCreated(userId, cmd.user),
           AccountCreated(userId, Account(accountNumber, "scala-bank-1024", 0)))

}

trait EventSourcing { userBusinessLogic: UserBusinessLogic =>

  def buildStateForUser: List[Event] => UserId => Option[User] =
    events => userId =>
      events.filter(_.id == userId).foldRight(None:Option[User])((event, maybeUser) => event match {
        case UserCreated(_, user) => Some(user)
        case EmailChanged(_, email) => maybeUser.map(_.copy(email = Some(email)))
        case _ => maybeUser
      })

  def ucs: User => Command => List[Event] =
    initial => {
      case cu: CreateUser => ucCreateUser(cu)(UUID.randomUUID().toString)(UUID.randomUUID().toString)
      case ce: ChangeEmail => ucChangeEmail(ce)
      case _ => Nil
    }

//  def store: User => Event => User = current => {
//    case UserCreated(_, user) => User(user.firstName, user.lastName, user.phone, user.email)
//    case EmailChanged(_, email) => current.copy(email = Some(email))
//  }
  
}

trait UserFunctions{ userEventSourcing: EventSourcing =>

  def createUser: User => UserId =
    user => {
      val created = ucs(user)(CreateUser(user))
      EsMock.save(created)
      created.head.id
    }

  def getUser: UserId => Option[User] =
    userId =>
      buildStateForUser(EsMock.saved)(userId)

  def changeEmail: UserId => Option[String] => Unit =
    userId => email => {
      val state = buildStateForUser(EsMock.saved)(userId).get
      val emailChanged = ucs(state)(ChangeEmail(userId, email))
      EsMock.save(emailChanged)
    }
}

object UserFunctions extends UserFunctions with EventSourcing with UserBusinessLogic

// f(events) -> state
// f(state, command) -> event
// f(state, event) -> state
