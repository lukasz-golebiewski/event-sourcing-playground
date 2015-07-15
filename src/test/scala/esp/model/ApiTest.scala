package esp.model

import org.scalatest.{FlatSpec, Matchers}

case class User(firstName: String, lastName: String, phone: Option[String], email: Option[String])

case class Account()

class ApiTest extends FlatSpec with Matchers {

  object api {
    type UserId = String
    type AccountNumber = String

    def getUser(id: api.UserId): Option[User] = ???

    def listAccounts(id: UserId): Seq[AccountNumber] = ???

    def create(user: User): UserId = ???
  }

  val user = User("Adam", "Szkoda", Some("555-CALL-ME-ADAM"), Some("adaszko@adaszko.com"))

  "User" should "be possible to create" in {
    api.create(user) should not be ""
  }

  it should "be possilbe to get user" in {
    val id = api.create(user)
    api.getUser(id) should not be None
  }

  it should "come with exactly one (main) account" in {
    api.listAccounts(api.create(user)) should have size (1)
  }
}
