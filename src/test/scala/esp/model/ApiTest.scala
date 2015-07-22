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

    def createUser(user: User): UserId = ???

    def changeEmail(id: UserId, email: Option[String]): Unit = ???
  }

  val user = User("Adam", "Szkoda", Some("555-CALL-ME-ADAM"), Some("john@doe.com"))

  "User" should "be possible to create" in {
    api.createUser(user) should not be ""
  }

  it should "be possilbe to get user" in {
    val id = api.createUser(user)
    api.getUser(id) should not be None
  }

  it should "come with exactly one (main) account" in {
    api.listAccounts(api.createUser(user)) should have size (1)
  }

  it should "be possible to change email" in {
    val id = api.createUser(user)
    val newEmail = Some("john_2@doe.com")
    api.changeEmail(id, newEmail)

    api.getUser(id).get.email shouldBe newEmail
  }

  it should "be NOT possible to change email" in {
    val id = api.createUser(user)
    val wrongMail = Some("john_2doe.com")
    api.changeEmail(id, wrongMail)

    api.getUser(id).get.email shouldNot be(wrongMail)
    api.getUser(id).get.email shouldBe user.email
  }
}
