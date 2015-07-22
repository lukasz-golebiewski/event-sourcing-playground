package esp.model

import org.scalatest.{FlatSpec, Matchers}

case class User(firstName: String, lastName: String, phone: Option[String], email: Option[String])

case class Account(number: String, name: String)

class ApiTest extends FlatSpec with Matchers {

  object api {

    type UserId = String
    type AccountNumber = String

    def getUser(id: UserId): Option[User] = ???

    def listAccounts(id: UserId): Seq[AccountNumber] = ???

    def createUser(user: User): UserId = ???

    def changeEmail(id: UserId, email: Option[String]): Unit = ???

    def setAccountName(accountNumber: AccountNumber, newName: String): Unit = ???

    def getAccount(accountNumber: AccountNumber): Option[Account] = ???

    def createAccount(id: UserId): Unit = ???
  }

  val user = User("Adam", "Szkoda", Some("555-CALL-ME-ADAM"), Some("john@doe.com"))

  "User" should "be possible to create" in {
    api.createUser(user) should not be ""
  }

  it should "be possilbe to get user" in {
    val id = api.createUser(user)
    api.getUser(id) should not be None
    api.getUser(id).get shouldBe user
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

  "Account" should "be possible to assign name" in {
    val id = api.createUser(user)
    val accountNumber = api.listAccounts(id).head

    api.setAccountName(accountNumber, newName = "vacation")
    api.getAccount(accountNumber).get.name shouldBe "vacation"
  }

  it should "NOT be possible to change name to be empty" in {
    val id = api.createUser(user)
    val accountNumber = api.listAccounts(id).head

    api.setAccountName(accountNumber, newName = "vacation")
    api.setAccountName(accountNumber, newName = "")
    api.getAccount(accountNumber).get.name shouldBe "vacation"
  }

  it should "come with default name of account" in {
    val accountNumber = api.listAccounts(api.createUser(user)).head

    api.getAccount(accountNumber).get.name shouldBe "scala-bank-1024"
  }

  it should "create second account" in {
    val id = api.createUser(user)

    api.createAccount(id)
    api.listAccounts(id) should have size (2)
  }

  it should "NOT create more than 5 accounts" in {
    val id = api.createUser(user)

    (2 to 5).foreach { i =>
      api.createAccount(id)
      api.listAccounts(id) should have size i
    }

    api.createAccount(id)
    api.listAccounts(id) should have size 5
  }

}
