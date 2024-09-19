import cats._

import java.util.Date
//import cats.implicits._
import cats.instances.all._
import cats.syntax.all._


object Main extends App {
  println("Изучаем Cats!".toUpperCase)

  //////////// Show

  val showInt = Show.apply[Int]
  val showString = Show.apply[String]

  println(showInt.show(123))
  println(showString.show("wdwdwd"))
  println(5678.show)

  implicit val dateShow: Show[Date] =
    (date: Date) => s"${date.getTime}ms since the epoch."

  println(new Date().show)

  final case class Cat(name: String, age: Int, color: String)

  implicit val catShow: Show[Cat] = Show.show[Cat] { cat =>
    val name = cat.name.show
    val age = cat.age.show
    val color = cat.color.show

    s"$name is a $age year-old $color cat."
  }

  println(Cat("Garfield", 38, "gingerandblack").show)

  ///////////// Eq

  //val eqInt: Eq[Int] = Eq[Int]

  println(Eq[Int].eqv(123, 123))
  println(Eq[Int].eqv(123, 122353))

  println(125 === 125)
  println(1 =!= 1)
  println(Option(10) === Option.empty[Int])
  println(Option(10) === Option(10))
  println(Option(10) === Option(20))
  println(10.some =!= 10.some)
  println(10.some === none[Int])

  implicit val dateEq: Eq[Date] =
    Eq.instance[Date] { (d1, d2) =>
      d1.getTime === d2.getTime
    }

  val x = new Date()
  Thread.sleep(1)
  val y = new Date()

  println(x === x)
  println(x === y)

  val cat1 = Cat("Garfield", 38, "orangeandblack")
  val cat2 = Cat("Heathcliff", 33, "orangeandblack")
  val optionCat1 = Option(cat1)
  val optionCat2 = Option.empty[Cat]

  implicit val catEqual: Eq[Cat] =
    Eq.instance[Cat] { (c1, c2) =>
      (c1.name === c2.name) &&
        (c1.age === c2.age) &&
        (c1.color === c2.color)
    }

  val cat11 = Cat("Garfield",   38,"orangeandblack")

  val cat22 = Cat("Heathcliff", 32,"orangeandblack")

  println(cat11 === cat22)
  println(cat11 =!= cat22)

  val optionCat11 = Option(cat1)
  val optionCat22 = Option.empty[Cat]

  println(optionCat11 === optionCat22)

}
