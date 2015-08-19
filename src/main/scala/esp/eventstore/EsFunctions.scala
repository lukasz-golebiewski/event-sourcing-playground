package esp.eventstore

import java.util.UUID

import esp.model.{UserId, User}

object EsMock {

  var users: List[UserEvent] = Nil

  def save(event: UserEvent) = users = event :: users

}

sealed trait UserEvent{ def id: UserId }
case class UserCreated(override val id: UserId, user: User) extends UserEvent

object UserFunctions {

  private def createUser: User => UserCreated = user => UserCreated(UUID.randomUUID().toString, user)

  def ucCreateUser: User => UserId = { user =>
    val userCreated = createUser(user)
    EsMock.save(userCreated)
    userCreated.id
  }

  private def getAllUserEvents: List[UserEvent] = EsMock.users
  private def getUserEvents: (UserId, List[UserEvent]) => List[UserEvent] = (userId, events) => events.filter(_.id == userId)
  private def getUser: List[UserEvent] => Option[User] = events => {
    var returnUser: Option[User] = None
    events.foreach {
      case UserCreated(_, user) => returnUser = Some(user)
    }
    returnUser
  }

  def ucGetUser: UserId => Option[User] = userId => getUser(getUserEvents(userId, getAllUserEvents))

}

// f(state, command) -> event
// f(state, event) -> state
// f(events) -> state
