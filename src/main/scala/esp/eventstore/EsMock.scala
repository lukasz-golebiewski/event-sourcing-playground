package esp.eventstore

object EsMock {
  var users: List[UserEvent] = Nil
  def save(events: List[UserEvent]) = users = events ::: users
}
