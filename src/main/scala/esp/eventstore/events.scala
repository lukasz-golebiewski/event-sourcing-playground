package esp.eventstore

import esp.model._

sealed trait Event{ def id: UserId }
sealed trait Command

sealed trait UserEvent extends Event
case class UserCreated(override val id: UserId, user: User) extends UserEvent
case class EmailChanged(override val id: UserId, email: String) extends UserEvent

sealed trait UserCommand extends Command
case class CreateUser(user: User) extends UserCommand
case class ChangeEmail(userId: UserId, email: Option[String]) extends UserCommand