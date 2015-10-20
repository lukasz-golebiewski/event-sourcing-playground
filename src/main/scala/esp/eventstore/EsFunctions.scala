package esp.eventstore

import java.util.UUID

import esp.model.{UserId, User}

import scalaz._
import Scalaz._

object EsMock {
  var users: List[UserEvent] = Nil
  def save(events: List[UserEvent]) = users = events ::: users
}

sealed trait Event{ def id: UserId }
sealed trait Command

sealed trait UserEvent extends Event
case class UserCreated(override val id: UserId, user: User) extends UserEvent
case class EmailChanged(override val id: UserId, email: String) extends UserEvent

sealed trait UserCommand extends Command
case class CreateUser(user: User) extends UserCommand
case class ChangeEmail(userId: UserId, email: Option[String]) extends UserCommand

trait UserBusinessLogic {

  def ucChangeEmail: ChangeEmail => List[UserEvent] =
    cmd =>
      (for {
        email <- cmd.email.toSuccess("empty email")
        validated <- UserValidation.validate(email)
      } yield EmailChanged(cmd.userId, validated)).toList

  def ucCreateUser: CreateUser => List[UserEvent] = cmd => List(UserCreated(UUID.randomUUID().toString, cmd.user))

}

trait UserEventSourcing { userBusinessLogic: UserBusinessLogic =>

  def buildStateForUser: List[UserEvent] => UserId => Option[User] = events => userId =>
      events.filter(_.id == userId).foldRight(None:Option[User])((event, maybeUser) => event match {
        case UserCreated(_, user) => Some(user)
        case EmailChanged(_, email) => maybeUser.map(_.copy(email = Some(email)))
      })

  def ucs: User => UserCommand => List[UserEvent] = initial => {
    case cu: CreateUser => ucCreateUser(cu)
    case ce: ChangeEmail => ucChangeEmail(ce)
  }

//  def store: User => UserEvent => User = current => {
//    case UserCreated(_, user) => User(user.firstName, user.lastName, user.phone, user.email)
//    case EmailChanged(_, email) => current.copy(email = Some(email))
//  }
  
}

trait UserFunctions{ userEventSourcing: UserEventSourcing =>

  def createUser: User => UserId = { user =>
    val created = ucs(user)(CreateUser(user))
    EsMock.save(created)
    created.head.id
  }

  def getUser: UserId => Option[User] = { userId =>
    buildStateForUser(EsMock.users)(userId)
  }

  def changeEmail(userId: UserId, email: Option[String]): Unit = {
    val state = buildStateForUser(EsMock.users)(userId).get
    val emailChanged = ucs(state)(ChangeEmail(userId, email))
    EsMock.save(emailChanged)
  }
}

object UserFunctions extends UserFunctions with UserEventSourcing with UserBusinessLogic

// f(events) -> state
// f(state, command) -> event
// f(state, event) -> state
