package esp.eventstore

import java.util.UUID

import esp.model._

trait AccountBusinessLogic {

  def ucCreateAccount: List[AccountWithUserId] => CreateAccount => List[Event] =
    state => cmd =>
      (for {
        validated <- AccountValidation.doesntExceed5Accounts(state)
      } yield AccountCreated(cmd.userId, Account(UUID.randomUUID().toString, s"name${state.length+1}", 0))).toList

  def ucChangeName: ChangeAccountName => List[Event] =
    cmd =>
      (for {
        validated <- AccountValidation.nonEmptyName(cmd.name)
      } yield AccountNameChanged("", cmd.accountNumber, validated)).toList

  def ucDepositMoney: List[AccountWithUserId] => DepositMoney => List[Event] =
    state => cmd =>
      List(MoneyDeposited(state.head.userId, None, cmd.accountNumber, cmd.amount))

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
          val transaction = UUID.randomUUID().toString
          List(MoneyDeposited(from.userId, Some(transaction), from.account.number, deductAmount),
               MoneyDeposited(to.userId, Some(transaction),  to.account.number, cmd.amount))
        }).toList.flatten
    }

}

trait AccountEventSourcing { accountBL: AccountBusinessLogic =>

  private def buildState: (Event, List[AccountWithUserId]) => List[AccountWithUserId] =
    (event, accounts) => event match {
      case AccountCreated(userId, account) => AccountWithUserId(account, userId) :: accounts
      case AccountNameChanged(_, accNum, name) => accounts.map(a => if (a.account.number == accNum) a.copy(account = a.account.copy(name = name)) else a)
      case MoneyDeposited(_, _, accNum, amount) => accounts.map(a => if (a.account.number == accNum) a.copy(account = a.account.copy(balance = a.account.balance + amount)) else a)
      case _ => accounts
    }

  def buildStateWithUserId: List[Event] => UserId => List[AccountWithUserId] =
    events => userId =>
      events.filter(_.id == userId).foldRight(Nil:List[AccountWithUserId])(buildState)

  def buildStateWithAccountNumber: List[Event] => AccountNumber => List[AccountWithUserId] =
    events => accNum =>
      events.foldRight(Nil:List[AccountWithUserId])(buildState).filter(_.account.number == accNum)

  def ucs: List[AccountWithUserId] => Command => List[Event] =
    initial => {
      case ca: CreateAccount => ucCreateAccount(initial)(ca)
      case can: ChangeAccountName => ucChangeName(can)
      case dm: DepositMoney => ucDepositMoney(initial)(dm)
      case tm: TransferMoney => ucTransferMoney(initial)(tm)
      case _ => Nil
    }
}

trait AccountFunctions { accountES: AccountEventSourcing =>

  def createAccount: UserId => Unit =
    userId => {
      val state = buildStateWithUserId(EsMock.saved)(userId)
      val created = ucs(state)(CreateAccount(userId))
      EsMock.save(created)
    }

  def setAccountName: AccountNumber => String => Unit =
    accNum => name => {
      val state = buildStateWithAccountNumber(EsMock.saved)(accNum)
      val changed = ucs(state)(ChangeAccountName("", accNum, name))
      EsMock.save(changed)
    }

  def getAccount: AccountNumber => Option[Account] =
    accNum =>
      buildStateWithAccountNumber(EsMock.saved)(accNum).find(_.account.number == accNum).map(_.account)

  def listAccount: UserId => Seq[AccountNumber] =
    userId =>
      buildStateWithUserId(EsMock.saved)(userId).map(_.account.number)

  def depositMoney: AccountNumber => BigDecimal => Unit =
    accNum => amount => {
      val state = buildStateWithAccountNumber(EsMock.saved)(accNum)
      val deposited = ucs(state)(DepositMoney(accNum, amount))
      EsMock.save(deposited)
    }

  def transferMoney: AccountNumber => AccountNumber => BigDecimal => Unit =
    from => to => amount => {
      val fromAcc = buildStateWithAccountNumber(EsMock.saved)(from)
      val toAcc = buildStateWithAccountNumber(EsMock.saved)(to)
      val transferred = ucs(fromAcc ::: toAcc)(TransferMoney(from, to, amount))
      EsMock.save(transferred)
    }

  def listTransaction: UserId => Seq[Transaction] =
    userId => {
      EsMock.saved.filter(_.id == userId).flatMap{
        case MoneyDeposited(_, Some(tr), accNum, amount) if amount < 0 => List(Tr(Some(accNum), None, Some(tr), amount))
        case MoneyDeposited(_, Some(tr), accNum, amount) if amount >= 0 => List(Tr(None, Some(accNum), Some(tr), amount))
        case MoneyDeposited(_, None, accNum, amount) => List(Tr(None, Some(accNum), None, amount))
        case _ => Nil
      }.groupBy(x => x.transaction).flatMap{
        case (Some(trNum), trs) => List(trs.reduce{ (a1, a2) => (a1, a2) match {
          case (Tr(Some(x), None, tr, am), Tr(None, Some(y), _, _)) => Tr(Some(x), Some(y), tr, am)
          case (Tr(None, Some(y), _, _), Tr(Some(x), None, tr, am)) => Tr(Some(x), Some(y), tr, am)
        }})
        case (None, trs) => trs
      }.map(tr => Transaction(tr.from, tr.to.get, tr.amount))
    }.toSeq

}

object AccountFunctions extends AccountFunctions with AccountEventSourcing with AccountBusinessLogic


