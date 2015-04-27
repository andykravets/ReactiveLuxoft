import java.util.Date

import rx.core.{Rx, Var}

import scala.util.Random

class ReactiveSpec extends UnitSpec {
  val random = new Random()

  //Domain model
  case class Trade(date: Date, notional: Double, counterparty: Counterparty, clearingHouse: ClearingHouse, defaultTrader: String = "InvestmentBank")

  case class ClearingHouse(name: String, counterparty: Counterparty)

  case class Counterparty(name: String, trader: Trader)

  case class Trader(name: String)

  case class Geek(name: String, age: Int, money: Option[Int], girlfriend: Option[Girlfriend])

  case class Girlfriend(name: String, dog: Option[Dog])

  case class Dog(nick: String)

  "Curryng" should "be fun" in {
    def unCurried(a: Int, b: Int): Int = a+b

    unCurried(2,2) should be (4)

    def curried(a: Int) = {
      ???
    }

    curried(2)(2) should be (4)

    def curried2(a: Int) = ???

    curried2(2)(2) should be (4)
  }


  "Monoid" should "be fun" in {

    trait Semigroup[F] {
      def append(f1: F, f2: => F): F
    }

    implicit object DatePlus extends Semigroup[Date] {
      def append(f1: Date, f2: => Date): Date = new Date(f1.getTime + f2.getTime)
    }

    def plus[A](l: A, r: A)(implicit s: Semigroup[A]) = s.append(l, r)

    plus(new Date(1), new Date(2)) should be(new Date(3))

    /*def sum[A](ls: Seq[A])(implicit s: Semigroup[A]) =
      ls.foldLeft(0)(s.append(_, _).tupled) */


    trait Monoid[A] extends Semigroup[A] {
      def zero: A
    }

    implicit object DateMonoid extends Monoid[Date] {
      def append(f1: Date, f2: => Date): Date = new Date(f1.getTime + f2.getTime)

      def zero: Date = new Date(0)
    }

    def sum[A](ls: Seq[A])(implicit m: Monoid[A]): A =
      ls.foldLeft(m.zero)((a, e) => m.append(a, e))

    sum(List(new Date(1), new Date(2))) should be(new Date(3))
  }


  val trades = (1 to 10).map(x => generateTrade())
  "Trades list" should "throw NPE" in {
    intercept[NullPointerException] {
      trades.map(_.clearingHouse).map(_.counterparty).map(_.name)
    }
  }

  "Trades list that" should "not throw NPE" in {
    val names = trades map (_.clearingHouse) filter (_ != null) map (_.counterparty) filter (_ != null) map (_.name)
    names.size should be < trades.size
    names.foreach(name =>
      name should not be null
    )
  }

  "Monad approach" should "be ok" in {
    val geeks = (1 to 20).map(_ => geekGenerator())

    geeks foreach {
      _ should not be null
    }

    geeks map (_.girlfriend) flatMap { gf => gf flatMap (x => x.dog) } foreach { dog =>
      println(dog)
      dog should not be null
    }
  }

  //You have to rewrite traders and fill random fields with Options and then check it in the test
  "Monad approach your turn" should "be ok for traders" in {

  }

  "Non propagation exaple" should "be starange" in {
    var a = 10
    var b = 20
    var c = a + b
    c should be(30)

    a = 20
    //No propagation
    c should be(30)
    //Do propagation by hand and it taste like a pain
    c = a + b
    c should be(40)
  }


  "Scala Rx first step" should "simple propagation" in {
    val a = Var(1)
    val b = Var(2)
    val c = Rx {
      a() + b()
    }
    c() should be(3)

    a() = 4
    c() should be(6)
  }

  "Scala Rx more complex" should "calculos propagation example" in {
    val a = Var(1)
    val b = Var(2)
    val c = Rx {
      a() + b()
    }
    val d = Rx {
      c() * 5
    }
    val e = Rx {
      c() + 4
    }
    val f = Rx {
      d() + e() + 4
    }
    f() should be(26)

    a() = 3
    f() should be(38)
  }

  //Generators
  private def geekGenerator(): Geek = {

    def girlGenerator(): Girlfriend = {
      val name = s"GirlFriend# ${random.nextInt(100)}"
      val dog = if (random.nextBoolean()) Some(Dog(random.nextString(10))) else None
      Girlfriend(name, dog)
    }

    val name = s"Geek# ${random.nextInt(10)}"
    val age = random.nextInt(78) + 12
    val money = if (random.nextBoolean()) Some(random.nextInt(10000000)) else None
    val hasGirl = random.nextBoolean()
    val gf = if (hasGirl) Some(girlGenerator()) else None
    Geek(name, age, money, gf)
  }


  private def generateTrade(): Trade = {
    val trader = Trader("Some trader")
    val counterparty = if (random.nextBoolean())
      Counterparty("Other investment bank", trader)
    else null
    val clearingHouse = if (random.nextBoolean())
      ClearingHouse("London cleraing house", counterparty)
    else null
    Trade(date = new Date(),
      notional = random.nextDouble(),
      counterparty,
      clearingHouse)
  }

}
