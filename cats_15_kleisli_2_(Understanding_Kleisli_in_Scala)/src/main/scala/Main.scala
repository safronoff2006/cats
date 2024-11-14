import cats.data.Kleisli
import cats.implicits._

import scala.language.postfixOps


object Main extends App {

  println("Изучаем Cats!".toUpperCase)
  println("KLeisli")

  object part1 {
    def chapter1 = {

      // Понимание Kleisli в Scala
      // https://medium.com/@supermanue/understanding-kleisli-in-scala-9c42ec1a5977

      // Это первый из серии постов, посвященных пониманию типа данных с самым странным названием
      // во всех языках программирования, за исключением, возможно, Yoneda.
      // Недавно я обнаружил, что это на самом деле простой для понимания и довольно полезный инструмент,
      // поэтому в этом посте я познакомлю вас с основами, а в следующих покажу несколько примеров из реального мира.

      // resource/kleisli.webp

      // Прежде всего: мое объяснение может быть не очень хорошим, и, вероятно, будет неполным.
      // Для лучшего введения, чем это, я могу порекомендовать только
      // Cats Kleisli https://typelevel.org/cats/datatypes/kleisli.html и
      // sanj.ink https://sanj.ink/posts/2017-06-07-composing-monadic-functions-with-kleisli-arrows.html
      // (автора картинки выше)

      // Этот пример начинается с пары функций, parse и reciprocal,
      // которые переходят от примитивного типа (String, Int) к типу высшего порядка (Option[Int], Option[Double]).
      // Пока все хорошо.

      val parse: String => Option[Int] =
        s => if (s.matches("-?[0-9]+")) Some(s.toInt) else None

      val reciprocal: Int => Option[Double] =
        i => if (i != 0) Some(1.0 / i) else None

      // Базовые примеры использования этих функций.

      println(
        parse("7"),
        parse("casa"),
        reciprocal(7),
        reciprocal(0)
      )


      // Вот где начинается проблема. Если вы хотите скомпоновать обе функции, вы не можете использовать
      // compose или даже сделать это как переменную, как в оригинальных.
      // Вместо этого вам придется объявить функцию и начать вкладывать flatMap, что действительно вредит читаемости.

      //FUNCTION COMPOSITION
      def parseAndReciprocal(s: String): Option[Double] = parse(s).flatMap(reciprocal)

      println {
        parseAndReciprocal("7")
      }

      // Вот как объявляется Kleisli: тип с тремя параметрами типа и функцией «run».
      // Что это значит? Легко! Он инкапсулирует функцию, которая получает аргумент типа A и возвращает тип F[B], A => F[B].
      // Скоро мы увидим, почему это здорово.


      //USING KLEISLI
      // final case class Kleisli[F[_], A, B](val run : scala.Function1[A, F[B]])

      // Эта функция аналогична функции в строке 4, хотя объявлена как тип Kleisli.
      // Требуемый аргумент run объявлен как функция, которая получает строку S и возвращает Option[Int].
      // Конечно, параметром выполнения может быть уже существующая функция или анонимная функция.

      val parseKleisli: Kleisli[Option,String,Int] =
        Kleisli(parse)

      val reciprocalKleisli: Kleisli[Option, Int, Double] =
        Kleisli(reciprocal)

      // Использование переменной типа Kleisli такое же,
      // как если бы это была стандартная функция.

      println(
        parseKleisli("7"),
        parseKleisli("casa"),
        reciprocalKleisli(7),
        reciprocalKleisli(0)
      )

      //Хорошая вещь в Kleisli в том, что он предоставляет кучу полезных инструментов.
      // Например, andThen обеспечивает композицию.
      // В этом случае andThen обеспечивает композицию, поэтому полученная составная функция
      // может быть объявлена как переменная, как и исходные.

      val parseAndReciprocalKleisli: Kleisli[Option, String, Double] = parseKleisli andThen reciprocalKleisli

      println {
        parseAndReciprocalKleisli("7")
      }

      //Кстати, функцию run можно выполнить явно или вызывая Kleisli как функцию.

      println {
        parseKleisli.run("7")
      }
      println {
        parseKleisli("7")
      }

      // После этого краткого введения в Kleisli
      // (опять же, не забудьте заглянуть на Cats Kleisli и sanj.ink для более подробного описания)
      // в следующих постах я покажу некоторые из наиболее мощных возможностей этого класса и то,
      // как мы используем его в реальных приложениях.
    }
  }

  part1.chapter1

}


