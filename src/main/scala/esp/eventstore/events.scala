package esp.eventstore

import esp.model._

sealed trait Event{ def id: UserId }
case class UserCreated(override val id: UserId, user: User) extends Event
case class EmailChanged(override val id: UserId, email: String) extends Event
case class AccountCreated(override val id: UserId, account: Account) extends Event
case class AccountNameChanged(override val id: UserId, accountNumber: AccountNumber, name: String) extends Event
case class MoneyDeposited(override val id: UserId, transactionId: Option[String], accountNumber: AccountNumber, amount: BigDecimal) extends Event

sealed trait Command
case class CreateUser(user: User) extends Command
case class ChangeEmail(userId: UserId, email: Option[String]) extends Command
case class CreateAccount(userId: UserId, currentAccounts: List[Account]) extends Command
case class ChangeAccountName(userId: UserId, accountNumber: AccountNumber, name: String) extends Command
case class DepositMoney(accountNumber: AccountNumber, amount: BigDecimal) extends Command
case class TransferMoney(from: AccountNumber, to: AccountNumber, amount: BigDecimal) extends Command
