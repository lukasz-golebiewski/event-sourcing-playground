package esp.eventstore

import scalaz._
import Scalaz._

trait UserValidation {

  type Email = String

  private def nonEmptyEmail(email: Email) = if (email.nonEmpty) email.successNel else "empty email".failureNel
  private def emailContainsAt(email: Email) = if (email.contains('@')) email.successNel else "email doesn't contain '@'".failureNel

  def validate(email: Email): ValidationNel[String, Email] = (nonEmptyEmail(email) |@| emailContainsAt(email)) { (_, _) => email }
}

object UserValidation extends UserValidation