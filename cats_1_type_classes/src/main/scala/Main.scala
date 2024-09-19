import cats._
import cats.data._
import cats.syntax.all._

object Main extends App {
  println("Hello, World!".toUpperCase)

  /////////////////////////  JsonWriter  ///////////////////////////////

  sealed trait Json


  final case class JsObject(get: Map[String, Json]) extends Json

  final case class JsString(get: String) extends Json

  final case class JsNumber(get: Double) extends Json

  final case class JsOption[A](get: Option[A]) extends Json

  case object JsNull extends Json

  trait JsonWriter[A] {
    def write(value: A): Json
  }

  object JsonWriter {
    //суммонер метод
    def apply[A](implicit ev: JsonWriter[A]): JsonWriter[A] = ev
  }

  object Json {
    //имплисит параметр
    def toJson[A](value: A)(implicit w: JsonWriter[A]): Json = w.write(value)

    //context bound c implicitly
    def toJsonB[A: JsonWriter](value: A): Json =
      implicitly[JsonWriter[A]].write(value)

  }

  final case class Person(name: String, email: String)
  final case class Fio(name: String, father: Option[String], age: Option[Double] = None)

  object JsonWriterInstances {
    implicit val stringWriter: JsonWriter[String] = value => JsString(value)
    implicit val personWriter: JsonWriter[Person] = value => JsObject(
      Map(
        "name" -> JsString(value.name),
        "email" -> JsString(value.email)
      )
    )
    implicit val fioWriter: JsonWriter[Fio] = value => JsObject(
      Map(
        "name" -> JsString(value.name),
        "father" -> JsOption(value.father),
        "age" -> JsOption(value.age)
      )
    )

    implicit val numberWriter: JsonWriter[Double] = value => JsNumber(value)

    implicit def optionWriter[A] (implicit writer: JsonWriter[A]): JsonWriter[Option[A]] = {
        case Some(value) => writer.write(value)
        case None => JsNull
    }


  }

  object JsonSyntax {
    implicit class JsonWriterOps[A](value: A) {
      def toJson(implicit w: JsonWriter[A]): Json = w.write(value)
    }

    //context bound
    implicit class JsonWriterOpsS[A: JsonWriter](value: A) {
      //использование суммонера
      def toJsonS(): Json = JsonWriter[A].write(value)

    }

  }

  import JsonSyntax._
  import JsonWriterInstances._

  println(Json.toJsonB(Person("Валера", "safronoff2006@gmail.com")))
  println(Person("Валера", "safronoff2006@gmail.com").toJson)
  println("Бля".toJson)
  println(123D.toJson)
  println(Option("Валера").toJson)
  println(Option.empty[String].toJson)
  println(Fio("Валерий", Some("Евгеньевич"),Some(44)).toJson)
  println(Fio("Валерий", None).toJson)
  println(Fio("Валерий", Some("Евгеньевич"),Some(44)).toJsonS())

  /////////////////////// Printable Library ///////////////////////////

  trait Printable[A] {
    def format(value: A): String
  }

  object PrintableInst {
    implicit val stringPrintable: Printable[String] = (input: String) => input
    implicit val intPrintable: Printable[Int] = (input: Int) => input.toString
  }

  object Printable {
    //def format[A](input: A )(implicit p: Printable[A]): String = p.format(input)
    def format[A: Printable](input: A): String = implicitly[Printable[A]].format(input)

    def print[A](input: A)(implicit p: Printable[A]):Unit = println(p.format(input))
  }

  final case class Cat(name: String, age: Int, color: String)
  import PrintableInst._

  object Cat {

    implicit val catPrintable: Printable[Cat] = (cat: Cat) => {
      val name = Printable.format(cat.name)
      val age = Printable.format(cat.age)
      val color = Printable.format(cat.color)
      s"$name is a $age year-old $color cat."
    }
  }

  object PrintableSyntax {
    implicit class PrintableOps[A](value: A) {
      def format(implicit p: Printable[A]): String = p.format(value)
    }

    implicit class PrintableOps2[A: Printable](value: A) {
      def print: Unit = println( implicitly[Printable[A]].format(value) )
    }
  }

  import PrintableSyntax._
  val cat = Cat("Мирка", 1, "Белый")
  Printable.print(cat)
  println(cat.format)
  cat.print




}