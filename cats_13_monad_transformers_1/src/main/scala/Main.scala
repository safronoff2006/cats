



import cats._
import cats.implicits._

import scala.language.postfixOps


object Main extends App {

  println("Изучаем Cats!".toUpperCase)

  object part1 {
    def part1: Unit = {
      // Монады подобны буррито, что означает, что как только вы попробуете, вы обнаружите, что возвращаетесь к ним снова и снова.
      // Это не без проблем. Так же как буррито могут раздуть талию, монады могут раздуть кодовую базу из-за вложенных for-comprehensions.

      // Представьте, что мы взаимодействуем с базой данных. Мы хотим найти запись пользователя.
      // Пользователь может присутствовать или отсутствовать, поэтому мы возвращаем Option[User].
      // Наше взаимодействие с базой данных может быть прервано по многим причинам (проблемы с сетью, проблемы аутентификации и т. д.),
      // поэтому этот результат заключен в Either, что дает нам окончательный результат Either[Error, Option[User]].

      // Чтобы использовать это значение, мы должны сделать вложенные flatMap вызовы (или, что эквивалентно, for-comprehensions):

      /*

          def lookupUserName(id: Long): Either[Error, Option[String]] =
            for {
              optUser <- lookupUser(id)
            } yield {
              for { user <- optUser } yield user.name
            }

      */

      // Это быстро становится очень утомительным.

      // Упражнение: Составление монад

      // Возникает вопрос.
      // Имея две произвольные монады,
      // можем ли мы объединить их каким-либо образом, чтобы создать одну монаду?
      // То есть, комбинируются  ли монады ?
      // Мы можем попытаться написать код, но вскоре столкнемся с проблемами:

      import cats.syntax.applicative._ // for pure

      // Hypothetical example. This won't actually compile:


      def compose[M1[_]: Monad, M2[_]: Monad] = {
        type Composed[A] = M1[M2[A]]
        new Monad[Composed] {
          def pure[A](a: A): Composed[A] =
            a.pure[M2].pure[M1]

          def flatMap[A, B](fa: Composed[A]) (f: A => Composed[B]): Composed[B] =
            // Problem! How do we write flatMap?
            ???

          override def tailRecM[A, B](a: A)(f: A => Composed[Either[A, B]]): Composed[B] = ???
        }
      }

      // Невозможно написать общее определение, flatMap не зная ничего о M1 или M2.
      // Однако, если мы знаем что-то о той или иной монаде,
      // мы обычно можем завершить этот код.
      // Например, если мы исправим M2, то появится Option определение :flatMap

      type M1[A] = List[A]
      type M2[B] = Option[B]

      type Composed[A] = M1[M2[A]]

      def flatMap[A, B](fa: Composed[A])
                       (f: A => Composed[B]): Composed[B] =
        fa.flatMap(_.fold[Composed[B]](None.pure[M1])(f))

      // Обратите внимание, что определение выше использует None — специфическую Option концепцию,
      // которая не появляется в общем Monad интерфейсе.
      // Нам нужна эта дополнительная деталь для объединения Option с другими монадами.
      // Аналогично, есть вещи о других монадах, которые помогают нам писать составные flatMap методы для них.
      // Это идея, лежащая в основе монад-трансформеров:
      // Cats определяет трансформаторы для различных монад, каждый из которых предоставляет дополнительные знания,
      // необходимые для объединения этой монады с другими. Давайте рассмотрим несколько примеров.

      // Пример трансформации

      // Cats предоставляет трансформаторы для многих монад,
      // каждая из которых названа суффиксом T:
      // EitherT composes Either с другими монадами,
      // OptionT composes Option и т. д.

      // Вот пример, который использует OptionT для композиции List и Option.
      // Мы можем использовать OptionT[List, A], ListOption[A] для удобства названный псевдонимом ,
      // чтобы преобразовать List[Option[A]] в одну монаду:

      import cats.data.OptionT

      type ListOption[A] = OptionT[List, A]

      // Обратите внимание, как мы строим ListOption изнутри наружу:
      // мы передаем List тип внешней монады в качестве параметра в OptionT преобразователь для внутренней монады.

      // Мы можем создавать экземпляры ListOption с помощью OptionT конструктора или, что более удобно, с помощью pure:

      import cats.instances.list._     // for Monad


      val result1: ListOption[Int] = OptionT(List(Option(10), Option(20), None))

      val result2: ListOption[Int] = 32.pure[ListOption]

      val res1 = result1.flatMap { x =>
        result2.map { y =>
          x + y
        }
      }

      println{
        res1
      }
      // res1: OptionT[List, Int] = OptionT(List(Some(42), Some(52), None))

      // Это основа всех монадных преобразователей.
      // Объединенные методы map и flatMap позволяют нам использовать обе монады-компоненты
      // без необходимости рекурсивно распаковывать и переупаковывать значения на каждом этапе вычисления.

      // Теперь давайте рассмотрим API более подробно.


    }
  }


  part1.part1



}
