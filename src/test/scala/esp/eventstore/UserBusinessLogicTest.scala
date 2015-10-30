package esp.eventstore

import esp.model.User
import org.scalatest.{FlatSpec, Matchers}

class UserBusinessLogicTest extends FlatSpec with Matchers {

  val userLogic = new UserBusinessLogic {}
  val id = "abc123"
  val email = "john.doe@mail.com"
  val correctChangeEmailCommand = ChangeEmail(id, Some(email))

  "Change Email to correct one" should "generate list that contains one event" in {
    userLogic.ucChangeEmail(correctChangeEmailCommand).size shouldBe 1
  }

  it should "generate EmailChanged event" in {
    userLogic.ucChangeEmail(correctChangeEmailCommand).head should equal(EmailChanged(id, email))
  }

  "Change Email to incorrect one" should "generate empty event list for empty email" in {
    userLogic.ucChangeEmail(ChangeEmail(id, Some(""))) shouldBe empty
  }

  it should "generate empty event list for None email" in {
    userLogic.ucChangeEmail(ChangeEmail(id, None)) shouldBe empty
  }

  it should "generate empty event list for email without at sign" in {
    userLogic.ucChangeEmail(ChangeEmail(id, Some("something.at.pl"))) shouldBe empty
  }

  "Create User" should "always generate one event about user creation" in {
    val user = User("Chuck", "Norris", Some("800-CALL-ME-Chucky"), Some("chuck@chuck.com"))
    val id = "222"
    userLogic.ucCreateUser(CreateUser(user))("111")("222") should not be empty
    userLogic.ucCreateUser(CreateUser(user))(id)("777").head should equal(UserCreated(id, user))
  }

}
