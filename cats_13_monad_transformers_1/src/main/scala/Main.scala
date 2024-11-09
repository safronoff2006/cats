


import cats._
import cats.data._
import cats.implicits._

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}
import scala.language.postfixOps
import scala.util.{Failure, Success}


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


      def compose[M1[_] : Monad, M2[_] : Monad] = {
        type Composed[A] = M1[M2[A]]
        new Monad[Composed] {
          def pure[A](a: A): Composed[A] =
            a.pure[M2].pure[M1]

          def flatMap[A, B](fa: Composed[A])(f: A => Composed[B]): Composed[B] =
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

      import cats.instances.list._ // for Monad


      val result1: ListOption[Int] = OptionT(List(Option(10), Option(20), None))

      val result2: ListOption[Int] = 32.pure[ListOption]

      val res1 = result1.flatMap { x =>
        result2.map { y =>
          x + y
        }
      }

      println {
        res1
      }
      // res1: OptionT[List, Int] = OptionT(List(Some(42), Some(52), None))

      println("================================================")

      // Это основа всех монадных преобразователей.
      // Объединенные методы map и flatMap позволяют нам использовать обе монады-компоненты
      // без необходимости рекурсивно распаковывать и переупаковывать значения на каждом этапе вычисления.

      // Теперь давайте рассмотрим API более подробно.

      // Сложность импорта
      // Импорт в примерах кода выше намекает на то, как все связано воедино.
      // Импортируем, cats.syntax.applicative чтобы получить pure синтаксис.
      // pure требует неявного параметра типа Applicative[ListOption].
      // Мы еще не встречались Applicatives, но все Monads они тоже есть Applicatives,
      // поэтому мы можем проигнорировать эту разницу на данный момент.

      //  Для того, чтобы сгенерировать наш Applicative[ListOption] нам нужны экземпляры Applicative List и OptionT.
      //  OptionT — это тип данных Cats, поэтому его экземпляр предоставляется его сопутствующим объектом.
      //  Экземпляр for List берется из cats.instances.list.

    }
  }

  object part2 {
    def part2 = {
      // Монады-трансформеры у кошек

      // Каждый преобразователь монад — это тип данных, определенный в cats.data,
      // который позволяет нам обертывать стеки монад для создания новых монад.
      // Мы используем монады, которые мы построили с помощью Monad класса типов.
      // Основные концепции, которые мы должны охватить, чтобы понять преобразователи монад:

      // доступные классы трансформаторов;
      // как строить стеки монад с помощью трансформаторов;
      // как создавать экземпляры стека монад;
      // как разбить стек на части, чтобы получить доступ к обернутым монадам.

      // Классы-трансформеры монад
      // По соглашению, в Cats монада Foo будет иметь класс-трансформер, называемый FooT.
      // Фактически, многие монады в Cats определяются путем объединения монады-трансформера с Id монадой.
      // Конкретно, некоторые из доступных экземпляров:

      // cats.data.OptionT для Option;
      // cats.data.EitherT для Either;
      // cats.data.ReaderT для Reader;
      // cats.data.WriterT для Writer;
      // cats.data.StateT для State;
      // cats.data.IdT для Id монады.

      // Стрелки Клейсли

      // В разделе 4.8 мы упоминали, что Reader монада является специализацией более общей концепции,
      // называемой «стрелкой Клейсли», представленной в Cats как cats.data.Kleisli.

      // Теперь мы можем раскрыть, что Kleisli и ReaderT, по сути, одно и то же!
      // ReaderT на самом деле является псевдонимом типа для Kleisli.
      // Следовательно, мы создавали Readers в последней главе и видели Kleislis в консоли.

      // Создание стеков монад

      // Все эти монадные преобразователи следуют одному и тому же соглашению.
      // Сам преобразователь представляет внутреннюю монаду в стеке,
      // в то время как первый параметр типа определяет внешнюю монаду.
      // Остальные параметры типа — это типы, которые мы использовали для формирования
      // соответствующих монад.
      // Например, наш ListOption тип выше является псевдонимом для,
      // OptionT[List, A] но результат фактически является List[Option[A]].
      // Другими словами, мы строим стеки монад изнутри наружу:

      type ListOption[A] = OptionT[List, A]

      // Многие монады и все трансформаторы имеют как минимум два параметра типа,
      // поэтому нам часто приходится определять псевдонимы типов для промежуточных стадий.

      // Например, предположим, что мы хотим обернуть Either вокруг Option.
      // Option — это самый внутренний тип, поэтому мы хотим использовать OptionT монадный трансформатор.
      // Нам нужно использовать Either в качестве первого параметра типа.
      // Однако Either сам по себе имеет два параметра типа, а монады — только один.
      // Нам нужен псевдоним типа, чтобы преобразовать конструктор типа в правильную форму:

      // Alias Either to a type constructor with one parameter:
      type ErrorOr[A] = Either[String, A]

      // Build our final monad stack using OptionT:
      type ErrorOrOption[A] = OptionT[ErrorOr, A]

      // ErrorOrOption является монадой, как и ListOption. Мы можем использовать pure, map и flatMap
      // как обычно для создания и преобразования экземпляров:


      import cats.instances.either._ // for Monad

      val a: ErrorOrOption[Int] = 10.pure[ErrorOrOption]
      // a: ErrorOrOption[Int] = OptionT(Right(Some(10)))
      val b: ErrorOrOption[Int] = 32.pure[ErrorOrOption]
      // b: ErrorOrOption[Int] = OptionT(Right(Some(32)))

      val c: OptionT[ErrorOr, Int] = a.flatMap(x => b.map(y => x + y))
      // c: OptionT[ErrorOr, Int] = OptionT(Right(Some(42)))
      println(a)
      println(b)
      println(c)
      println("----------------")

      // Все становится еще более запутанным, когда мы хотим сложить три или более монад.

      // Например, давайте создадим Future из Either на Option.
      // Мы снова строим это изнутри наружу с из OptionT из EitherT для Future.
      // Однако мы не можем определить это в одной строке, потому что EitherT
      // имеет три параметра типа:

      /*
      case class EitherT[F[_], E, A](stack: F[Either[E, A]]) {
        // etc...
      }
      */

      // Три параметра типа следующие:

      // F[_] — внешняя монада в стеке ( Either— внутренняя);
      // E - тип ошибки для Either;
      // A — это тип результата для Either.

      // На этот раз мы создаем псевдоним EitherT для фиксации Future и Error позволяя варьировать A:

      import scala.concurrent._
      import cats.syntax.applicative._ // for pure
      import cats.instances.future._ // for Monad
      import scala.concurrent.ExecutionContext.Implicits.global

      type FutureEither[A] = EitherT[Future, String, A]
      type FutureEitherOption[A] = OptionT[FutureEither, A]

      // Наш гигантский стек теперь состоит из трех монад, а наши методы map и flatMap
      // прорезают три уровня абстракции:


      val futureEitherOr1: FutureEitherOption[Int] =
        for {
          a <- 10.pure[FutureEitherOption]
          b <- 32.pure[FutureEitherOption]
        } yield a + b

      val futureEitherOr2: FutureEitherOption[Int] =
        for {
          a <- 10.pure[FutureEitherOption]
          b <- 32.pure[FutureEitherOption].map(_ * 2)
        } yield a + b


      val futureEitherOr3: FutureEitherOption[Int] =
        for {
          a <- 10.pure[FutureEitherOption].flatMap { i =>
            OptionT {
              EitherT.right[String](Future(Option(i + 1)))
              // или
              /*
                    EitherT {
                      Future[Either[String, Option[Int]]] {
                        Right(Some(i + 1))
                      }
                    }
              */

            }
          }
          b <- 32.pure[FutureEitherOption].map(_ * 2)
        } yield a + b

      val futureEitherOr4: FutureEitherOption[Int] =
        for {
          a <- 10.pure[FutureEitherOption].flatMap { i =>
            OptionT {
              /*
                  EitherT {
                    Future[Either[String, Option[Int]]] {
                      Left("Что то не так")
                    }
                  }
              */
              // или
              EitherT.left[Option[Int]](Future("Что то совсем не так"))
            }


          }
          b <- 32.pure[FutureEitherOption].map(_ * 2)
        } yield a + b

      val futureEitherOr5: FutureEitherOption[Int] =
        for {
          a <- 10.pure[FutureEitherOption].flatMap { i =>
            OptionT {
              EitherT {
                Future[Either[String, Option[Int]]] {
                  throw new Throwable("ошибка!")
                  Left("Что то не так")
                }
              }
            }
          }
          b <- 32.pure[FutureEitherOption].map(_ * 2)
        } yield a + b


      futureEitherOr1.value.value onComplete {
        case Failure(exception) => println(exception.getMessage)
        case Success(value) => println(value)
      }


      futureEitherOr2.value.value onComplete {
        case Failure(exception) => println(exception.getMessage)
        case Success(value) => println(value)
      }


      futureEitherOr3.value.value onComplete {
        case Failure(exception) => println(exception.getMessage)
        case Success(value) => println(value)
      }

      futureEitherOr4.value.value onComplete {
        case Failure(exception) => println(exception.getMessage)
        case Success(value) => println(value)
      }

      futureEitherOr5.value.value onComplete {
        case Failure(exception) => println(exception.getMessage)
        case Success(value) => println(value)
      }


    }
  }

  object part3 {
    def part3 = {
      // Kind Projector (Проектор типов)

      // Если вы часто обнаруживаете, что определяете несколько псевдонимов типов при построении стеков монад,
      // вы можете попробовать плагин компилятора Kind Projector.
      // Kind Projector улучшает синтаксис типов Scala,
      // чтобы упростить определение частично применяемых конструкторов типов. Например:

      // import cats.instances.option._ // for Monad // for Monad

      val m1 = 123.pure[EitherT[Option, String, *]]
      // res3: EitherT[Option, String, Int] = EitherT(Some(Right(123)))

      println {
        m1.value
      }
      // Kind Projector не может упростить все объявления типов до одной строки,
      // но он может сократить количество промежуточных определений типов, необходимых для того,
      // чтобы наш код оставался читаемым.
    }
  }

  object part4 {
    def part4 = {
      // Создание и распаковка экземпляров

      // Как мы видели выше, мы можем создавать преобразованные стеки монад,
      // используя соответствующий метод преобразователя монад applyили обычный pure синтаксис :

      // Create using apply:
      type ErrorOr[A] = Either[String, A]
      val errorStack1 = OptionT[ErrorOr, Int](Right(Some(10)))
      // errorStack1: OptionT[ErrorOr, Int] = OptionT(Right(Some(10)))
      println {
        errorStack1
      }

      // Create using pure:
      type ErrorOrOption[A] = OptionT[ErrorOr, A]
      val errorStack2 = 32.pure[ErrorOrOption]
      // errorStack2: ErrorOrOption[Int] = OptionT(Right(Some(32)))
      println {
        errorStack2
      }

      // После того, как мы закончили со стеком трансформеров монад,
      // мы можем распаковать его с помощью его value метода.
      // Это возвращает нетрансформированный стек.
      // Затем мы можем манипулировать отдельными монадами обычным способом:

      println {
        // Extracting the untransformed monad stack:
        errorStack1.value
        // res4: ErrorOr[Option[Int]] = Right(Some(10))
      }

      println {
        // Mapping over the Either in the stack:
        errorStack2.value.map(_.getOrElse(-1))
        // res5: Either[String, Int] = Right(32)
      }

      // Каждый вызов value распаковывает один трансформер монад.
      // Нам может понадобиться более одного вызова, чтобы полностью распаковать большой стек.
      // Например, для Await стека FutureEitherOption выше нам нужно вызвать value дважды:

      type FutureEither[A] = EitherT[Future, String, A]
      type FutureEitherOption[A] = OptionT[FutureEither, A]

      import scala.concurrent.ExecutionContext.Implicits.global


      val futureEitherOr: FutureEitherOption[Int] =
        for {
          a <- 10.pure[FutureEitherOption]
          b <- 32.pure[FutureEitherOption]
        } yield a + b

      val intermediate: FutureEither[Option[Int]] = futureEitherOr.value

      val stack: Future[Either[String, Option[Int]]] = intermediate.value



      println(Await.result(stack, 1.second))
    }
  }

  object part5 {
    def part5 = {
      // Экземпляры по умолчанию

      // Многие монады в Cats определяются с использованием соответствующего трансформатора и Id монады.
      // Это обнадеживает, поскольку подтверждает, что API для монад и трансформаторов идентичны.
      // Reader, Writer, и State все определяются таким образом:

      /*
        type Reader[E, A] = ReaderT[Id, E, A] // = Kleisli[Id, E, A]
        type Writer[W, A] = WriterT[Id, W, A]
        type State[S, A]  = StateT[Id, S, A]
       */

      // В других случаях монадные преобразователи определяются отдельно от соответствующих им монад.
      // В этих случаях методы преобразователя имеют тенденцию отражать методы монады.
      // Например, OptionT определяет getOrElse, и EitherT определяет fold, bimap, swap, и другие полезные методы.

      // Модели использования

      // Широкое использование монадных трансформаторов иногда затруднено,
      // поскольку они объединяют монады вместе предопределенными способами.
      // Без тщательного обдумывания мы можем оказаться вынужденными
      // распаковывать и перепаковывать монады в различных конфигурациях, чтобы оперировать ими в различных контекстах.

      // Мы можем справиться с этим несколькими способами.
      // Один из подходов заключается в создании единого «суперстека»
      // и его использовании во всей нашей кодовой базе.
      // Это работает, если код прост и в значительной степени однороден по своей природе.
      // Например, в веб-приложении мы могли бы решить, что все обработчики запросов являются асинхронными
      // и все могут давать сбой с одним и тем же набором кодов ошибок HTTP.
      // Мы могли бы разработать собственный ADT, представляющий ошибки,
      // и использовать слияние Future и Either везде в нашем коде:

      sealed abstract class HttpError
      final case class NotFound(item: String) extends HttpError
      final case class BadRequest(msg: String) extends HttpError
      // etc...

      type FutureEither[A] = EitherT[Future, HttpError, A]

      // Подход «суперстека» начинает терпеть неудачу в более крупных, более разнородных кодовых базах,
      // где разные стеки имеют смысл в разных контекстах.

      // Другой шаблон проектирования, который имеет больше смысла в этих контекстах,
      // использует монадные преобразователи в качестве локального «связующего кода».
      // Мы раскрываем непреобразованные стеки на границах модулей, преобразуем их для локальной работы с ними и
      // депреобразуем их перед передачей дальше.
      // Это позволяет каждому модулю кода принимать собственные решения о том, какие преобразователи использовать:

      import cats.data.Writer

      type Logged[A] = Writer[List[String], A]

      // Methods generally return untransformed stacks:
      def parseNumber(str: String): Logged[Option[Int]] =
        util.Try(str.toInt).toOption match {
          case Some(num) => Writer(List(s"Read $str"), Some(num))
          case None      => Writer(List(s"Failed on $str"), None)
        }

      //val otpn: OptionT[Logged, Int] = OptionT(parseNumber("10"))
      // какой тип выводит компилятор если OptionT apply Logged[Option[Int]]

      // Consumers use monad transformers locally to simplify composition:
      def addAll(a: String, b: String, c: String): Logged[Option[Int]] = {
        import cats.data.OptionT

        val result = for {
          a <- OptionT(parseNumber(a))
          b <- OptionT(parseNumber(b))
          c <- OptionT(parseNumber(c))
        } yield a + b + c

        result.value
      }

      // This approach doesn't force OptionT on other users' code:
      val result1 = addAll("1", "2", "3")
      // result1: Logged[Option[Int]] = WriterT(
      //   (List("Read 1", "Read 2", "Read 3"), Some(6))
      // )

      println {
        result1
      }

      val result2 = addAll("1", "a", "3")
      // result2: Logged[Option[Int]] = WriterT(
      //   (List("Read 1", "Failed on a"), None)
      // )

      println {
        result2
      }

      // К сожалению, не существует универсальных подходов к работе с монадными преобразователями.
      // Лучший подход для вас может зависеть от множества факторов:
      // размера и опыта вашей команды, сложности вашей кодовой базы и т. д.
      // Возможно, вам придется поэкспериментировать и собрать отзывы коллег,
      // чтобы определить, подходят ли вам монадные преобразователи.

    }
  }

  object part6 {
    def part6 = {
      // Упражнение: Монады: Преобразование и Развертывание

      // Автоботы, известные роботы в маскировке, часто отправляют сообщения во время боя,
      // запрашивая уровни мощности своих товарищей по команде.
      // Это помогает им координировать стратегии и проводить сокрушительные атаки.
      // Метод отправки сообщений выглядит следующим образом:

      /*
          def getPowerLevel(autobot: String): Response[Int] = ???
      */

      // Передача данных занимает время в вязкой атмосфере Земли,
      // и сообщения иногда теряются из-за сбоев в работе спутников
      // или саботажа со стороны надоедливых десептиконов Responses.
      // Поэтому они представлены в виде стека монад:

     /*
        type Response[A] = Future[Either[String, A]]
     */


      // Оптимус Прайм устал от вложенных в его нейронную матрицу осмыслений.
      // Помогите ему, переписав Response с помощью монадного трансформатора.

      // Это относительно простая комбинация.
      // Мы хотим Future снаружи и Either внутри,
      // поэтому мы строим изнутри наружу, используя EitherT из Future:
      /*
            import cats.data.EitherT
            import scala.concurrent.Future

            type Response[A] = EitherT[Future, String, A]
       */



      // Теперь протестируйте код, реализуя getPowerLevel извлечение данных
      // из набора воображаемых союзников. Вот данные, которые мы будем использовать:
      /*
          val powerLevels = Map(
                "Jazz"      -> 6,
                "Bumblebee" -> 8,
                "Hot Rod"   -> 10
              )
      */



      // Если автобота нет на powerLevels карте, вернуть сообщение об ошибке,
      // сообщающее, что они были недоступны. Включите name в сообщение для лучшего эффекта.

      import cats.data.EitherT
      import scala.concurrent.Future
      val powerLevels = Map(
        "Jazz"      -> 6,
        "Bumblebee" -> 8,
        "Hot Rod"   -> 10
      )

      import cats.instances.future._ // for Monad
      import scala.concurrent.ExecutionContext.Implicits.global

      type Response[A] = EitherT[Future, String, A]

      def getPowerLevel(ally: String): Response[Int] = {
        powerLevels.get(ally) match {
          case Some(avg) => EitherT.right(Future(avg))
          case None      => EitherT.left(Future(s"$ally unreachable"))
        }
      }

      // Два автобота могут выполнить специальный ход, если их общий уровень мощности больше 15.
      // Напишите второй метод, canSpecialMove который принимает имена двух союзников и проверяет,
      // возможен ли специальный ход. Если какой-либо союзник недоступен, выдайте сообщение об ошибке:

    }
  }

  //part1.part1
  //part2.part2
  //part3.part3
  //part4.part4
  //part5.part5
  part6.part6

}
