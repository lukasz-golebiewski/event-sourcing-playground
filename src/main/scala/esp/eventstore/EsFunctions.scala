package esp.eventstore

import java.util.UUID

import esp.model.{UserId, User}

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

trait UserEventSourcing {

  def buildStateForUser: List[UserEvent] => UserId => Option[User] = events => userId =>
      events.filter(_.id == userId).foldRight(None.asInstanceOf[Option[User]])((event, maybeUser) => event match {
        case UserCreated(_, user) => Some(user)
        case EmailChanged(_, email) => maybeUser.map(_.copy(email = Some(email)))
      })


  def uc: User => UserCommand => List[UserEvent] = initial => {
    case CreateUser(user) => List(UserCreated(UUID.randomUUID().toString, user))
    case ChangeEmail(userId, maybeEmail) => if (maybeEmail.nonEmpty && maybeEmail.get.contains('@')) List(EmailChanged(userId, maybeEmail.get)) else Nil
  }

  def store: User => UserEvent => User = current => {
    case UserCreated(_, user) => User(user.firstName, user.lastName, user.phone, user.email)
    case EmailChanged(_, email) => current.copy(email = Some(email))
  }


  def createUser: User => UserId = { user =>
    val created = uc(user)(CreateUser(user))
    EsMock.save(created)
    created.head.id
  }

  def ucGetUser: UserId => Option[User] = { userId =>
    buildStateForUser(EsMock.users)(userId)
  }

  def changeEmail(userId: UserId, email: Option[String]): Unit = {
    val state = buildStateForUser(EsMock.users)(userId).get
    val changed = uc(state)(ChangeEmail(userId, email))
    EsMock.save(changed)
  }
  
}

object UserFunctions extends UserEventSourcing

// f(events) -> state
// f(state, command) -> event
// f(state, event) -> state
