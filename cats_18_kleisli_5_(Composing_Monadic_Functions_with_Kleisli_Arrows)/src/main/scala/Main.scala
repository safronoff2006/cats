import cats.Monad
import cats.data.Kleisli
import cats.implicits._

import scala.language.postfixOps


object Main extends App {

  println("Изучаем Cats!".toUpperCase)
  println("KLeisli")

  object part1 {
    def chapter1 = {
      // Композиция монадических функций с помощью стрелок Клейсли

      // Композиция функций — это здорово, не правда ли?
      // Это один из краеугольных камней функционального программирования.
      // Имея функцию g: A => B и функцию, f: B => C мы можем скомпоновать их (объединить их вместе),
      // чтобы f compose g вернуть функцию A => C. Композиция скрывает промежуточные шаги A => B и B => C,
      // вместо этого позволяя нам сосредоточиться на начальном вводе (A) и конечном выводе (C).
      // Это клей, который позволяет нам писать много маленьких функций и объединять их в более крупные, более полезные функции.

      // resources/function-composition.jpg

      // Композиция функций работает справа налево, где первая вызываемая функция находится справа.
      // Это может сбивать с толку при изучении композиции, так как мы привыкли читать слева направо.
      // Если вы находите это сбивающим с толку, вы можете использовать функцию andThen,
      // которая упорядочивает функции слева направо: g andThen f в отличие от f compose g.

      // В этой статье мы используем язык Scala http://www.scala-lang.org/
      // и библиотеку функционального программирования Cats http://typelevel.org/cats/
      // для иллюстрации основных концепций. Исходный код для этой статьи доступен на Github.
      // https://github.com/ssanj/kleisli

      // Чтобы пояснить это на простом примере, начнем со следующих функций:

      def mul2: Int => Int = _ * 2

      def power2: Int => Double = Math.pow(_, 2)

      def doubleToInt: Double => Int = _.toInt

      def intToString: Int => String = _.toString

      // Хотя эти простые функции работают изолированно, мы также можем объединить их (составить) вместе,
      // чтобы создать более мощную функцию, которая делает то же, что и все функции:

      val pipeline: Int => String = intToString compose mul2 compose doubleToInt compose power2

      println {
        pipeline(3)
        //returns "18"
      }


      // Функция конвейера объединяет все функции вместе для создания новой функции, которая:
      // 1.Возводит указанное число в степень 2.
      // 2.Преобразует результат в значение типа Int.
      // 3.Умножает полученное значение на 2
      // 4. Преобразует результат в строку

      // Мы можем это сделать, поскольку типы выравниваются по всему периметру:
      // Int => Double //power2
      //        Double => Int //doubleToInt
      //                  Int => Int //mul2
      //                         Int => String //intToString

      // Int => String //pipeline

      // Теперь мы можем использовать и передавать функцию конвейера,
      // не задумываясь обо всех мелких функциях, входящих в ее состав.

      // Монадические функции

      // Все становится немного интереснее, когда у нас есть функции, возвращающие значения в контексте:

      def stringToNonEmptyString: String => Option[String] = value =>
        if (value.nonEmpty) Option(value) else None

      def stringToNumber: String => Option[Int] = value =>
        if (value.matches("-?[0-9]+")) Option(value.toInt) else None

      // Если мы попытаемся объединить stringToNonEmptyString и stringToNumber :

      // val pipeline2: String => Option[Int] = stringToNumber compose stringToNonEmptyString

      // мы получаем следующую ошибку компиляции:

      // [error]  found   : String => Option[String]
      // [error]  required: String => String
      // [error]     val pipeline: String => Option[Int] = stringToNumber compose stringToNonEmptyString

      // О, боже! Когда мы составляем stringToNonEmptyString с stringToNumber,
      // функция stringToNumber ожидает String, но вместо этого stringToNonEmptyString предоставляет ей Option[String].
      // Типы больше не выравниваются, и мы не можем составить:

      // the types don't align
      // String => Option[String] //stringToNonEmptyString
      //           String => Option[Int] //stringToNumber

      // Было бы неплохо, если бы нам не приходилось думать о контексте типа результата (Option[String] в данном случае)
      // и просто продолжать компоновку на основе простого типа (String в данном случае).

      // Композиция Клейсли
      // Kleisli — это тип Стрелки https://wiki.haskell.org/Arrow_tutorial#Kleisli_Arrows
      // для Монадического https://wiki.haskell.org/Monad  контекста. Он определяется как:

      /*
        final case class Kleisli[F[_], A, B](run: A => F[B])
      */

      // resources/kleisli-type.jpg

      // Тип Kleisli — это обертка вокруг A => F[B], где F — некоторый контекст, который является Монадой.
      // Что помогает нам с нашей композицией контекстных результатов, так это то,
      // что у Kleisli есть функция компоновки со следующей сигнатурой (упрощенной для ясности):

      /*
          def compose(g: A => F[B], f: B => F[C])(implicit M: Monad[F]): A => F[C]
      */

      // Приведенная выше сигнатура говорит нам о том, что мы можем объединить функции,
      // возвращающие результаты в контексте F (для которого у нас есть экземпляр Monad),
      // с функциями, которые работают с простым неконтекстуализированным значением:

      // A => F[B] //g
      //        B => F[C] //f

      // A => F[C] //f compose g

      // resources/kleisli-composition.jpg

      // Для функций stringToNonEmptyString и stringToNumber используется монадический контекст Option
      // (обе функции возвращают необязательное значение).

      // Так почему же методу композиции Kleisli нужен экземпляр Monadic для F?
      // Под капотом композиция Kleisli использует Monadic bind ( >>= ) для объединения значений Monadic.
      // Bind определяется как:

      /*
            def bind[A, B](fa: F[A])(f: A => F[B]): F[B]
       */


      // Использование композиции Клейсли
      // Давайте попробуем снова скомпоновать функции stringToNonEmptyString и stringToNumber, но на этот раз
      // с использованием композиции Клейсли:

      val stringToNonEmptyStringK: Kleisli[Option, String, String] = Kleisli(stringToNonEmptyString) //Kleisli[Option, String, String]
      val stringToNumberK: Kleisli[Option, String, Int] = Kleisli(stringToNumber) //Kleisli[Option, String, Int]

      val pipeline3: Kleisli[Option, String, Int] = stringToNumberK compose stringToNonEmptyStringK //Kleisli[Option, String, Int]

      println(
        pipeline3("1000"), //Some(1000)
        pipeline3(""), //None
        pipeline3("A12B") //None
      )

      // И теперь мы можем успешно скомпоновать две функции!
      // Кроме того, обратите внимание, как при использовании разных входных данных изменяется результат Monadic;
      // для компоновки этих значений Monadic через композицию Kleisli применяются те же правила, что и для Monadic bind.
      // Если значение None возвращается из одной из промежуточных функций, конвейер возвращает None.
      // Если все функции завершаются успешно со значениями Some, конвейер также возвращает Some.

      println("---------------------------------")
    }

    def chapter2 = {
      // Использование простых монад
      // Учитывая, что композиция Kleisli нуждается в экземпляре Monadic, чтобы сделать свое волшебство,
      // можем ли мы просто заменить композицию Kleisli на прямые Monads? Давайте попробуем:

      def stringToNonEmptyString: String => Option[String] = value =>
        if (value.nonEmpty) Option(value) else None

      def stringToNumber: String => Option[Int] = value =>
        if (value.matches("-?[0-9]+")) Option(value.toInt) else None

      val pipeline: String => Option[Int] = Option(_) >>= stringToNonEmptyString >>= stringToNumber
      println(
        pipeline("1000"), //Some(1000)
        pipeline(""),// None
        pipeline("A12B")// None
      )

      // Или если у нас есть вводные данные заранее:

      println(
        Option("1000") >>= stringToNonEmptyString >>= stringToNumber , //Some(1000)
        Option("") >>= stringToNonEmptyString >>= stringToNumber , //None
        Option("A12B") >>= stringToNonEmptyString >>= stringToNumber //None
      )

      // И, похоже, мы можем это сделать.

      // Преимущества композиции Kleisli

      // Так что же на самом деле дает нам композиция Kleisli по сравнению с использованием старых добрых монад?

      // 1.Позволяет программировать в более композиционном стиле.
      // 2.Абстрагирует возвышение ценностей до Монады.

      // А если прищуриться, A => F[B] очень похоже на Монаду Reader.
      // Подробнее об этом позже.

    }
  }

  part1.chapter1
  part1.chapter2
}


