


import cats._
import cats.data._
import cats.implicits._

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}
import scala.language.postfixOps
import scala.util.{Failure, Success, Try}
import scala.concurrent.ExecutionContext.Implicits.global

object Main extends App {

  println("Изучаем Cats!".toUpperCase)
  println("Effective Error Handling With EitherT in Scala")

  object part1 {

    case class User(id: String)

    def part1: Unit = {
      // Введение

      // Обработка ошибок является важнейшим аспектом любого языка программирования,
      // и Scala предлагает несколько способов управления ею.
      // Среди наиболее часто используемых инструментов для обработки ошибок — тип Either,
      // а также альтернативы вроде Try. Однако использование Either напрямую может стать сложным при работе с вложенными типами,
      // такими как Future, IO или другими эффективными контекстами.

      // В этом уроке мы рассмотрим, как EitherT упрощает и улучшает обработку ошибок в Scala,
      // делая ее более компонуемой и удобной для работы в таких сценариях.

      // Either для обработки ошибок
      // Как упоминалось выше, Either — один из наиболее распространенных методов обработки ошибок в Scala.

      // Простой сценарий обработки ошибок
      // Давайте определим простой сценарий, в котором мы используем Either для обработки ошибок:

      case class User(id: String)

      def getUserProfile(userId: String): Either[String, User] = ???
      def calculateDiscount(user: User): Either[String, Double] = ???
      def placeOrder(itemId:String, discount:Double, user: User): Either[String, String] = ???
      def performAction(userId: String, itemId: String): Either[String, String] = for {
        user <- getUserProfile(userId)
        discount <- calculateDiscount(user)
        orderId <- placeOrder(itemId, discount, user)
      } yield orderId

      // В этом примере у нас есть три функции, которые потенциально могут потерпеть неудачу,
      // поэтому каждая возвращает Either для представления успеха или неудачи.
      // Используя for-comprehension, мы можем элегантно скомпоновать эти функции в один рабочий процесс,
      // что упрощает выполнение всего действия, одновременно обрабатывая любые возможные ошибки на этом пути.





    }

    def part2 = {
      // Сложные сценарии
      // Пока все работает отлично и элегантно.
      // Однако, поскольку эти операции включают вызовы базы данных и сети,
      // нам нужно сделать их асинхронными с помощью Future, IO или чего-то подобного.
      // Давайте перепишем код, чтобы использовать Future в качестве типа эффекта для обработки этих асинхронных операций:

      def getUserProfile(userId: String): Future[Either[String, User]] = ???
      def calculateDiscount(user: User): Future[Either[String, Double]] = ???
      def placeOrder(itemId:String, discount:Double, user: User): Future[Either[String, String]] = ???

      // В этом случае мы имеем дело с вложенными монадами.
      // Одна представляет асинхронную природу с помощью Future,
      // а другая представляет потенциальные сбои с помощью Either.

      // Поскольку методы теперь возвращают Future, мы не можем составить эти функции с помощью одного for-comprehension.
      // Давайте рассмотрим возможный способ справиться с этим:

      def performAction(userId: String, itemId: String): Future[Either[String, String]] = {
        for {
          userEither <- getUserProfile(userId)
          result <- userEither match {
            case Left(error) => Future.successful(Left(error))
            case Right(user) =>
              for {
                discountEither <- calculateDiscount(user)
                orderResult <- discountEither match {
                  case Left(error) => Future.successful(Left(error))
                  case Right(discount) => placeOrder(itemId, discount, user)
                }
              } yield orderResult
          }
        } yield result
      }

      // Как мы видим, такой подход значительно увеличивает объем кода,
      // необходимого для правильной обработки ошибок.
      // Более того, по мере роста числа таких функций сложность и многословность кода могут еще больше возрасти,
      // что усложнит его чтение и поддержку. Вот тут-то и приходит на помощь EitherT .

      // EitherT
      // EitherT — это преобразователь монад из библиотеки Cats,
      // который расширяет возможности типа Either, позволяя комбинировать его с другими монадами,
      // такими как Future , Option , IO и т. д .
      // Вместо того, чтобы вручную разворачивать и управлять несколькими монадами,
      // преобразователи монад позволяют нам сглаживать эти слои в единую структуру, которую можно легко комбинировать.

      // Создание экземпляра
      // Сигнатура типа для EitherT представлена как EitherT[F[_], L, R] ,
      // где F — конструктор типа, который указывает на внешнюю монаду, такую как Future, IO и т. д.
      // В этой сигнатуре L представляет тип левого значения, аналогичного Left в Either,
      // часто используемого для сообщений об ошибках.
      // Аналогично R представляет тип правого значения, аналогичного Right в Either,
      // что указывает на успешные результаты.

      // Давайте рассмотрим, как создать экземпляр EitherT.
      // В этом разделе мы будем использовать Try для типа F.
      // Но это работает так же для Future, IO и других:

      val opValue: Try[Either[String, Int]] = Try(Right(100))
      val response: EitherT[Try, String, Int] = EitherT(opValue)

      // Поскольку EitherT — это простой класс case,
      // мы можем просто обернуть значение для создания экземпляра.


      // Если мы уже знаем, что значение равно Right ,
      // мы можем напрямую поднять его до EitherT с помощью функций liftF() или right() :

      val num1: EitherT[Try, String, Int] = EitherT.right[String](Try(100))
      val num2: EitherT[Option, Nothing, Int] = EitherT.liftF(Option(2))
      val num22: EitherT[Option, Nothing, Nothing] = EitherT.liftF(None)
      println(num1.value, num2.value, num22.value)

      // Аналогично мы можем использовать функцию left() для переноса ошибки в EitherT .
      val num3: EitherT[Try, String, Int] = EitherT.left[Int](Try("Данные не найдены"))
      println {
        num3.value
      }

      // Кроме того, EitherT предоставляет удобные методы, такие как fromOption() и fromEither() ,
      // которые позволяют перенести Option или Either непосредственно в экземпляр EitherT без их ручной упаковки.

      val num4: EitherT[Try, String, Int] = EitherT.fromOption[Try](Some(10), "Данных нет")
      val num5: EitherT[Try, Nothing, Int] = EitherT.fromEither[Try](Right(14))
      val num6: EitherT[Try, String, Nothing] = EitherT.fromOption[Try](None, "Данных нет")
      val num7: EitherT[Try, String, Nothing] = EitherT.fromEither[Try](Left("ошибка"))

      println(num4, num5, num6, num7)

      // Функции преобразования
      // Поскольку это монада, мы можем использовать такие функции, как map , flatMap и т. д. на экземпляре EitherT .
      // Подобно Either , эти функции работают напрямую с типом R,
      // без необходимости вручную обрабатывать внешний тип эффекта F.
      // Это упрощает работу с асинхронными или эффективными вычислениями.
      // Кроме того, мы можем использовать функцию leftMap() для работы с типом L.
      // Чтобы извлечь базовое значение из экземпляра EitherT , мы можем использовать функцию value() :

      val trValue: Try[Either[String, Int]] = Try(Right(100))
      val resp: EitherT[Try, String, Int] = EitherT(trValue)
      val mappedValue: EitherT[Try, String, Int] = response.map(_ * 5)
      val underlying: Try[Either[String, Int]] = mappedValue.value
      println {
        underlying
      }
      val failureCase: EitherT[Try, String, Int] =  EitherT[Try, String, Int](Try(Left("invalid number!")))
      println {
        failureCase
      }
      println {
        failureCase.leftMap(_.toUpperCase).value
      }

      // Функция bimap() позволяет нам применять
      // отдельные функции для обработки как успешных, так и ошибочных случаев:

      val eithert1: EitherT[Try, String, Int] =  EitherT[Try, String, Int](Try(Right(100)))
      val eithert2: EitherT[Try, String, Int] =  EitherT[Try, String, Int](Try(Left("ой!")))

      def biMappedRes(eithert: EitherT[Try, String, Int]): EitherT[Try, String, Int] =
        eithert.bimap(e => e.toUpperCase, s => s * 5)

      println(
        biMappedRes(eithert1).value,
        biMappedRes(eithert2).value
      )

      // Поскольку это монада, мы можем использовать for-comprehension
      // для легкого объединения нескольких экземпляров EitherT :

      val num1_1: EitherT[Try, String, Int] =  EitherT[Try, String, Int](Try(Right(100)))
      val num2_2: EitherT[Try, String, Int] =  EitherT[Try, String, Int](Try(Right(2)))
      val divRes: EitherT[Try, String, Int] = for {
        n1 <- num1_1
        n2 <- num2_2
        div = n1 / n2
      } yield div

      println {
        divRes.value
      }

      // Как показано здесь, работа с правым значением экземпляра EitherT проста и бесшовна.
      // Это позволяет нам сосредоточиться на логике нашего кода,
      // а не на том, как управлять вложенными монадами.


    }

    def part3 = {
      // Переписывание сложных сценариев с использованием EitherT

      // Чтобы переписать ранее обсуждавшийся сложный сценарий с использованием EitherT,
      // нам не нужно менять сигнатуру наших методов, которые извлекают данные.
      // Поэтому функции, которые мы определили ранее, останутся неизменными:

      def getUserProfile(userId: String): Future[Either[String, User]] = ???
      def calculateDiscount(user: User): Future[Either[String, Double]] = ???
      def placeOrder(itemId:String, discount:Double, user: User): Future[Either[String, String]] = ???

      // Теперь мы можем обернуть каждый из этих вызовов функций в EitherT
      // и использовать for-comprehension для их композиции:

      def performAction(userId: String, itemId: String): Future[Either[String, String]] = {
        val composeEitT: EitherT[Future, String, String] = for {
          user <- EitherT(getUserProfile(userId))
          discount <- EitherT(calculateDiscount(user))
          orderId <- EitherT(placeOrder(itemId, discount, user))
        } yield orderId

        composeEitT.value
      }

      // Как мы видим, код значительно упростился.
      // Теперь он очень похож на оригинальную версию, в которой не было Future.

    }

  }

  part1.part1
  part1.part2
  part1.part3

}

// Заключение

// В этой статье мы рассмотрели различные подходы к обработке ошибок в Scala,
// начав с базового использования Either для обработки ошибок в синхронных операциях.
// Затем мы представили эффекты вроде Future или IO и увидели,
// как они добавляют сложности и шаблонности при управлении асинхронными операциями в сочетании с обработкой ошибок.
// Это привело к представлению EitherT из библиотеки Cats,
// который значительно упрощает код при работе с вложенными эффектами вроде Future[Either].

// Используя EitherT, мы можем поддерживать чистую,
// компонуемую и лаконичную логику обработки ошибок даже в более сложных сценариях.
// Кроме того, мы рассмотрели различные полезные функции, предоставляемые EitherT,
// которые позволяют нам эффективно обрабатывать как успешные, так и неудачные случаи.
// Как всегда, пример кода, использованный в этой статье, доступен на GitHub
// https://github.com/Baeldung/scala-tutorials/tree/master/scala-libraries-fp

