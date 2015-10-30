package esp.eventstore

object EsMock {
  var saved: List[Event] = Nil
  def save(events: List[Event]) = saved = events ::: saved
}
