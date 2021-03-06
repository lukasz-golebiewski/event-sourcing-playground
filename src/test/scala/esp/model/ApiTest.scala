package esp.model

import esp.Api
import org.scalatest.{FlatSpec, Matchers}

trait ApiTest extends FlatSpec with Matchers { api:Api =>

  val user = User("Adam", "Smith", Some("555-CALL-ME-ADAM"), Some("john@doe.com"))
  val user2 = User("Marcin", "Kowalski", Some("555-CALL-ME-MARCIN"), Some("mbh@doe.com"))

  "User" should "be possible to create" in {
    api.createUser(user) should not be ""
  }

  it should "be possible to get user" in {
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

  it should "have default balance 0" in {
    val accountNumber = api.listAccounts(api.createUser(user)).head

    api.getAccount(accountNumber).get.balance shouldBe BigDecimal("0.00")
  }

  it should "be possible to deposit money" in {
    val accountNumber = api.listAccounts(api.createUser(user)).head
    val amount = BigDecimal("289.00")

    api.depositMoney(accountNumber, amount)
    api.getAccount(accountNumber).get.balance shouldBe amount
  }

  it should "be possible to transfer money between own accounts" in {
    val id = api.createUser(user)
    api.createAccount(id)

    val fromNumber = api.listAccounts(id).head
    val toNumber = api.listAccounts(id).last

    api.depositMoney(fromNumber, amount = BigDecimal(100))
    api.getAccount(fromNumber).get.balance shouldBe BigDecimal(100)
    api.getAccount(toNumber).get.balance shouldBe BigDecimal(0)

    api.transferMoney(fromNumber, toNumber, amount = BigDecimal(40))

    api.getAccount(fromNumber).get.balance shouldBe BigDecimal(60)
    api.getAccount(toNumber).get.balance shouldBe BigDecimal(40)
  }

  it should "NOT be possible to transfer money between own accounts if over limit" in {
    val id = api.createUser(user)
    api.createAccount(id)

    val fromNumber = api.listAccounts(id).head
    val toNumber = api.listAccounts(id).last

    api.depositMoney(fromNumber, amount = BigDecimal(100))
    api.getAccount(fromNumber).get.balance shouldBe BigDecimal(100)
    api.getAccount(toNumber).get.balance shouldBe BigDecimal(0)

    api.transferMoney(fromNumber, toNumber, amount = BigDecimal(2000))

    api.getAccount(fromNumber).get.balance shouldBe BigDecimal(100)
    api.getAccount(toNumber).get.balance shouldBe BigDecimal(0)
  }

  it should "list history of 0 transactions" in {
    val id = api.createUser(user)
    api.createAccount(id)

    api.listTransactionHistory(id) should have size 0
  }

  it should "list history of 1 transactions" in {
    val id = api.createUser(user)
    api.createAccount(id)

    val accountNumber = api.listAccounts(id).head
    val amount = BigDecimal("289.00")

    api.depositMoney(accountNumber, amount)

    val transactions = api.listTransactionHistory(id)
    transactions should have size 1

    transactions.head.from shouldBe None
    transactions.head.to shouldBe accountNumber
    transactions.head.amount shouldBe amount
  }

  "Bank" should " receive fee for money transfers between accounts of different users" in {
    val id = api.createUser(user)
    val id2 = api.createUser(user2)
    val accountNumber: AccountNumber = api.listAccounts(id).head
    api.depositMoney(accountNumber, 100)

    val initialBankBalance = Bank.Account.balance
    val transferAmount: BigDecimal = 10
    api.transferMoney(accountNumber, api.listAccounts(id2).head, transferAmount)
    Bank.Account.balance shouldBe initialBankBalance + Bank.transferFee(transferAmount)
  }

  it should " deduct transfer fee from origin account" in {
    val id = api.createUser(user)
    val id2 = api.createUser(user2)
    val accountNumber: AccountNumber = api.listAccounts(id).head
    val originInitialBalance: Int = 100
    api.depositMoney(accountNumber, originInitialBalance)

    val transferAmount: BigDecimal = 10
    api.transferMoney(accountNumber, api.listAccounts(id2).head, transferAmount)

    api.getAccount(accountNumber).get.balance shouldBe originInitialBalance - transferAmount - Bank.transferFee(transferAmount)
  }
}
