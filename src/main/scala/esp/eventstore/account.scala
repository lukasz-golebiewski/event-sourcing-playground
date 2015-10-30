package esp.eventstore

import java.util.UUID

import esp.model._

trait AccountBusinessLogic {

  def ucCreateAccount: List[Account] => CreateAccount => List[Event] =
    state => cmd =>
      (for {
        validated <- AccountValidation.doesntExceed5Accounts(state)
      } yield AccountCreated(cmd.userId, Account(UUID.randomUUID().toString, s"name${cmd.currentAccounts.length+1}", 0))).toList

  def ucChangeName: ChangeAccountName => List[Event] =
    cmd =>
      (for {
        validated <- AccountValidation.nonEmptyName(cmd.name)
      } yield AccountNameChanged("", cmd.accountNumber, validated)).toList

  def ucDepositMoney: DepositMoney => List[Event] =
    cmd =>
      List(MoneyDeposited("", cmd.accountNumber, cmd.amount))

  def ucTransferMoney: List[AccountWithUserId] => TransferMoney => List[Event] =
    state => cmd => {
      val fromAcc = state.find(_.account.number == cmd.from)
      val toAcc = state.find(_.account.number == cmd.to)
      (for {
        from <- fromAcc
        to <- toAcc
        if from.account.balance >= cmd.amount
        fee <- Some(Bank.transferFee(cmd.amount))
      } yield {
          val deductAmount = if (from.userId != to.userId) {
            Bank.Account = Bank.Account.copy(balance = Bank.Account.balance + fee)
            -cmd.amount - fee
          } else -cmd.amount
          List(MoneyDeposited("", from.account.number, deductAmount),
               MoneyDeposited("", to.account.number, cmd.amount))
        }).toList.flatten
    }

}

case class AccountWithUserId(account: Account, userId: UserId)

trait AccountEventSourcing { accountBL: AccountBusinessLogic =>

  def buildState: (Event, List[Account]) => List[Account] =
    (event, accounts) => event match {
        case AccountCreated(_, account) => account :: accounts
        case AccountNameChanged(_, accNum, name) => accounts.map(a => if (a.number == accNum) a.copy(name = name) else a)
        case MoneyDeposited(_, accNum, amount) => accounts.map(a => if (a.number == accNum) a.copy(balance = a.balance + amount) else a)
        case _ => accounts
      }

  def buildStateForAccountWithUserId: List[Event] => UserId => List[Account] =
    events => userId =>
      events.filter(_.id == userId).foldRight(Nil:List[Account])(buildState)

  def buildStateForAccountWithAccountNumber: List[Event] => AccountNumber => List[Account] =
    events => accNum =>
      events.foldRight(Nil:List[Account])(buildState).filter(_.number == accNum)

  def ucs: List[Account] => Command => List[Event] =
    initial => {
      case ca: CreateAccount => ucCreateAccount(initial)(ca)
      case can: ChangeAccountName => ucChangeName(can)
      case dm: DepositMoney => ucDepositMoney(dm)
      case _ => Nil
    }

  //TODO: refactor to remove duplicity
  def buildStateWithUserId: (Event, List[AccountWithUserId]) => List[AccountWithUserId] =
    (event, accounts) => event match {
      case AccountCreated(userId, account) => AccountWithUserId(account, userId) :: accounts
      case AccountNameChanged(_, accNum, name) => accounts.map(a => if (a.account.number == accNum) a.copy(account = a.account.copy(name = name)) else a)
      case MoneyDeposited(_, accNum, amount) => accounts.map(a => if (a.account.number == accNum) a.copy(account = a.account.copy(balance = a.account.balance + amount)) else a)
      case _ => accounts
    }

  def buildStateForAccountAndUserIdWithAccountNumber: List[Event] => AccountNumber => List[AccountWithUserId] =
    events => accNum =>
      events.foldRight(Nil:List[AccountWithUserId])(buildStateWithUserId).filter(_.account.number == accNum)

  def ucs2: List[AccountWithUserId] => Command => List[Event] =
    initial => {
      case tm: TransferMoney => ucTransferMoney(initial)(tm)
      case _ => Nil
    }
}

trait AccountFunctions { accountES: AccountEventSourcing =>

  def createAccount: UserId => Unit =
    userId => {
      val state = buildStateForAccountWithUserId(EsMock.saved)(userId)
      val created = ucs(state)(CreateAccount(userId, state))
      EsMock.save(created)
    }

  def setAccountName: AccountNumber => String => Unit =
    accNum => name => {
      val state = buildStateForAccountWithAccountNumber(EsMock.saved)(accNum)
      val changed = ucs(state)(ChangeAccountName("", accNum, name))
      EsMock.save(changed)
    }

  def getAccount: AccountNumber => Option[Account] =
    accNum =>
      buildStateForAccountWithAccountNumber(EsMock.saved)(accNum).find(_.number == accNum)

  def listAccount: UserId => Seq[AccountNumber] =
    userId =>
      buildStateForAccountWithUserId(EsMock.saved)(userId).map(_.number)

  def depositMoney: AccountNumber => BigDecimal => Unit =
    accNum => amount => {
      val state = buildStateForAccountWithAccountNumber(EsMock.saved)(accNum)
      val deposited = ucs(state)(DepositMoney(accNum, amount))
      EsMock.save(deposited)
    }

  def transferMoney: AccountNumber => AccountNumber => BigDecimal => Unit =
    from => to => amount => {
      val fromAcc = buildStateForAccountAndUserIdWithAccountNumber(EsMock.saved)(from)
      val toAcc = buildStateForAccountAndUserIdWithAccountNumber(EsMock.saved)(to)
      val transferred = ucs2(fromAcc ::: toAcc)(TransferMoney(from, to, amount))
      EsMock.save(transferred)
    }
}

object AccountFunctions extends AccountFunctions with AccountEventSourcing with AccountBusinessLogic
