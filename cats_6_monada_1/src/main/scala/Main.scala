


import cats._
import cats.implicits._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success, Try}
//import cats.instances.all._
//import cats.syntax.all._

import scala.language.postfixOps


object Main extends App {

  println("Изучаем Cats!".toUpperCase)

  object part1 {
    def part1: Unit = {
      def parseInt(str: String): Option[Int] =
        scala.util.Try(str.toInt).toOption

      def divide(a: Int, b: Int): Option[Int] =
        if (b == 0) None else Some(a / b)

      def stringDivideBy(aStr: String, bStr: String): Option[Int] =
        parseInt(aStr).flatMap { aNum =>
          parseInt(bStr).flatMap { bNum =>
            divide(aNum, bNum)
          }
        }

      println(stringDivideBy("6", "2"))
      println(stringDivideBy("6", "0"))
      println(stringDivideBy("6", "foo"))
      println(stringDivideBy("bar", "2"))

      def stringDivideBy2(aStr: String, bStr: String): Option[Int] =
        for {
          aNum <- parseInt(aStr)
          bNum <- parseInt(bStr)
          ans <- divide(aNum, bNum)
        } yield ans

      println(stringDivideBy2("6", "2"))
      println(stringDivideBy2("6", "0"))
      println(stringDivideBy2("6", "foo"))
      println(stringDivideBy2("bar", "2"))

      println {
        for {
          x <- (1 to 3).toList
          y <- (4 to 5).toList
        } yield (x, y)
      }

      def doSomethingLongRunning: Future[Int] = Future.successful(3)

      def doSomethingElseLongRunning: Future[Int] = Future.successful(4)

      def doSomethingVeryLongRunning: Future[Int] =
        for {
          result1 <- doSomethingLongRunning
          result2 <- doSomethingElseLongRunning
        } yield result1 + result2


      doSomethingVeryLongRunning.onComplete { res =>
        res match {
          case Failure(exception) => println(exception.getMessage)
          case Success(value) => println(value)
        }
      }
    }
  }

  object part2 {
    def part2(): Unit = {
      trait Monad[F[_]] {
        def pure[A](value: A): F[A]

        def flatMap[A, B](value: F[A])(func: A => F[B]): F[B]

        def map[A, B](value: F[A])(func: A => B): F[B] =
          flatMap(value)(a => pure(func(a)))
      }

    }
  }

  object part3 {
    def part3 = {
      val opt1 = Monad[Option].pure(3)
      val opt2 = Monad[Option].flatMap(opt1)(a => Some(a + 2))
      val opt3 = Monad[Option].map(opt2)(a => 100 * a)
      println(opt1)
      println(opt2)
      println(opt3)

      val list1 = Monad[List].pure(3)
      val list2 = Monad[List].flatMap(List(1, 2, 3))(a => List(a, a * 10))
      val list3 = Monad[List].map(list2)(a => a + 123)
      println(list1)
      println(list2)
      println(list3)

      Monad[Option].flatMap(Option(1))(a => Option(a * 2))
      Monad[List].flatMap(List(1, 2, 3))(a => List(a, a * 10))
      Monad[Vector].flatMap(Vector(1, 2, 3))(a => Vector(a, a * 10))

      val fm = Monad[Future]

      val future: Future[Int] = fm.flatMap(fm.pure(1))(x => fm.pure(x + 2))
      println {
        Await.result(future, 1 second)
      }

      println(1.pure[Option])
      println("efefe".pure[List])

      def sumSquare[F[_] : Monad](a: F[Int], b: F[Int]): F[Int] =
        a.flatMap(x => b.map(y => x * x + y * y))

      println {
        sumSquare(Option(3), Option(4))
      }

      println {
        sumSquare(List(1, 2, 3), List(4, 5))
      }

      def sumSquare2[F[_] : Monad](a: F[Int], b: F[Int]): F[Int] =
        for {
          x <- a
          y <- b
        } yield x * x + y * y

      println {
        sumSquare2(Option(3), Option(4))
      }

      println {
        sumSquare2(List(1, 2, 3), List(4, 5))
      }

      //sumSquare(3, 4)  не компилируется
      //No implicit arguments of type: Monad[ScalaNumberProxy]

      //package cats
      //type Id[A] = A

      println {
        sumSquare(3: Id[Int], 4: Id[Int])
      }
      println("Dave": Id[String])
      println(123: Id[Int])
      println(List(1, 2, 3): Id[List[Int]])


      //val a = 3
      //val b = 4
      // так не катит

      val a = Monad[Id].pure(3)
      // a: Id[Int] = 3
      val b = Monad[Id].flatMap(a)(_ + 1)
      // b: Id[Int] = 4


      println {
        val sum: Id[Int] = for {
          x <- a
          y <- b
        } yield x + y

        sum
      }

      def pure[A](value: A): Id[A] = value

      def map[A, B](initial: Id[A])(func: A => B): Id[B] = func(initial)

      def flatMap[A, B](initial: Id[A])(func: A => Id[B]): Id[B] = func(initial)
    }
  }

  object part4 {
    def part4: Unit = {
      val either1: Either[String, Int] = Right(10)
      val either2: Either[String, Int] = Right(32)
      val either0: Either[String, Int] = Left("Жопв")
      val either3: Either[String, Int] = Left("Хуйня")

      println {
        for {
          a <- either1
          b <- either2
        } yield a + b
      }

      println {
        for {
          a <- either1
          b <- either3
        } yield a + b
      }


      val a = 3.asRight[String]
      // a: Either[String, Int] = Right(3)
      val b = 4.asRight[String]
      // b: Either[String, Int] = Right(4)

      println {
        for {
          x <- a
          y <- b
        } yield x * x + y * y
      }

      println("------------------------------------------")
      //      def countPositive(nums: List[Int]) =
      //        nums.foldLeft(Right(0)) { (accumulator, num) =>
      //          if(num > 0) {
      //            accumulator.map(_ + 1) //Type mismatch. Required: Right[Nothing, Int], found: Either[Nothing, Int]
      //          } else {
      //            Left("Negative. Stopping!") //Type mismatch. Required: Right[Nothing, Int], found: Left[String, Nothing]
      //          } // Type mismatch. Required: (Right[Nothing, Int], Int) => Right[Nothing, Int], found: (Right[Nothing, Int], Int) => Either[String, Int]
      //        }
      // Не катит!

      def countPositive(nums: List[Int]) =
        nums.foldLeft(0.asRight[String]) { (accumulator, num) =>
          if (num > 0) {
            accumulator.map(_ + 1)
          } else {
            Left("Negative. Stopping!")
          }
        }

      println {
        countPositive(List(1, 2, 3))
      }

      println {
        countPositive(List(1, -2, 3))
      }

      println("------------------------------------------")

      println {
        Either.catchOnly[NumberFormatException]("foo".toInt)
      }
      println {
        Either.catchNonFatal(sys.error("Badness"))
      }

      println {
        Either.fromTry(Try("foo".toInt))
      }

      println {
        Either.fromOption[String, Int](None, "Badness")
      }

      println {
        "Error".asLeft[Int].getOrElse(0)
      }

      println {
        "Error".asLeft[Int].orElse(2.asRight[String])
      }

      println {
        "error".asLeft[Int].recover {
          case _: String => -1
        }
      }

      println {
        "error".asLeft[Int].recoverWith {
          case _: String => Right(-1)
        }
      }

      println("------------------------------------------")

      println {
        "foo".asLeft[Int].leftMap(_.reverse)
      }

      println {
        6.asRight[String].bimap(_.reverse, _ * 7)
      }

      println {
        "bar".asLeft[Int].bimap(_.reverse, _ * 7)
      }

      println {
        123.asRight[String]
      }

      println {
        123.asRight[String].swap
      }
    }
  }

  object part5 {
    def part5: Unit = {
      println("---- обработка ошибок -----")

      println {
        val aa = 10
        val bb = 0
        for {
          a <- aa.asRight[String]
          b <- bb.asRight[String]
          c <- if (b == 0) "DIV0".asLeft[Int]
          else (a / b).asRight[String]
        } yield c * 100

      }

      sealed trait LoginError extends Product with Serializable
      final case class UserNotFound(username: String) extends LoginError
      final case class PasswordIncorrect(username: String) extends LoginError
      case object UnexpectedError extends LoginError

      case class User(username: String, password: String)

      type LoginResult = Either[LoginError, User]

      def handleError(error: LoginError): Unit =
        error match {
          case UserNotFound(u) =>
            println(s"User not found: $u")

          case PasswordIncorrect(u) =>
            println(s"Password incorrect: $u")

          case UnexpectedError =>
            println(s"Unexpected error")
        }


      val result1: LoginResult = User("dave", "passw0rd").asRight
      val result2: LoginResult = UserNotFound("dave").asLeft

      result1.fold(handleError, println)
      result2.fold(handleError, println)

    }
  }

  object part6 {
    def part6: Unit = {
      println("---------- MonadError -----------")
      /*
      package cats
      trait MonadError[F[_], E] extends Monad[F] {
          // Поднять ошибку в контекст `F`:
          def raiseError[A](e: E): F[A]
          // Обрабатывать ошибку, потенциально устраняя ее:
          def handleErrorWith[A](fa: F[A])(f: E => F[A]): F[A]
          // Обрабатывать все ошибки, восстанавливаясь после них:
          def handleError[A](fa: F[A])(f: E => A): F[A]
          //Протестируйте экземпляр `F` ошибкой, если предикат не удовлетворен:
          def ensure[A](fa: F[A])(e: E)(f: A => Boolean): F[A]
      }
       */

      type ErrorOr[A] = Either[String, A]
      val monadErrorE: MonadError[ErrorOr, String] = MonadError[ErrorOr, String]

      val success1 = monadErrorE.pure(42)
      val failure1 = monadErrorE.raiseError("ОШИБКА!")
      val failure2 = monadErrorE.raiseError("Другая ОШИБКА!")
      val success2 = monadErrorE.pure("да")

      println {
        success1
      }

      println {
        failure1
      }

      println {
        monadErrorE.handleErrorWith(failure1) {
          case "ОШИБКА!" => monadErrorE.pure(666)
          case _ => monadErrorE.raiseError("Fail!")
        }
      }

      println("----------------")

      import Default._

      def handleEwith1[A](r: ErrorOr[A])(implicit default: Default[A]): ErrorOr[A] = monadErrorE.handleErrorWith(r) {
        case "ОШИБКА!" => monadErrorE.pure(default.get)
        case _ => monadErrorE.raiseError("Fail!")
      }

      def handleEwith2[A: Default](r: ErrorOr[A]): ErrorOr[A] = {
        monadErrorE.handleErrorWith(r) {
          case "ОШИБКА!" => monadErrorE.pure(implicitly[Default[A]].get)
          case _ => monadErrorE.raiseError("Fail!")
        }
      }

      def handleEwith3[A: Default](r: ErrorOr[A]): ErrorOr[A] = {
        monadErrorE.handleErrorWith(r) {
          case "ОШИБКА!" => monadErrorE.pure(Default[A].get)
          case _ => monadErrorE.raiseError("Fail!")
        }
      }


      println(handleEwith1[Int](success1))
      println(handleEwith1[Int](failure1))
      println(handleEwith1[String](failure2))
      println(handleEwith1[String](success2))
      println("----------------")

      val success3 = monadErrorE.pure(true)
      println(handleEwith1[Boolean](success3))
      println(handleEwith1[Boolean](failure1))
      println(handleEwith1[Boolean](failure2))

      println("----------------")

      println(handleEwith2[Int](success1))
      println(handleEwith2[Int](failure1))
      println(handleEwith2[String](failure2))
      println(handleEwith2[String](success2))
      println(handleEwith2[Boolean](success3))
      println(handleEwith2[Boolean](failure1))
      println(handleEwith2[Boolean](failure2))

      println("----------------")

      println(handleEwith3[Int](success1))
      println(handleEwith3[Int](failure1))
      println(handleEwith3[String](failure2))
      println(handleEwith3[String](success2))
      println(handleEwith3[Boolean](success3))
      println(handleEwith3[Boolean](failure1))
      println(handleEwith3[Boolean](failure2))


    }
  }

  object part7 {
    def part7: Unit = {

      type ErrorOr[A] = Either[String, A]
      val monadErrorE: MonadError[ErrorOr, String] = MonadError[ErrorOr, String]

      val success1 = monadErrorE.pure(42)
      val failure1 = monadErrorE.raiseError("ОШИБКА!")
      val failure2 = monadErrorE.raiseError("Другая ОШИБКА!")
      val success2 = monadErrorE.pure("да")
      val success3 = monadErrorE.pure(true)
      val success4 = monadErrorE.pure(5000)


      println {
        monadErrorE.handleError[Int](failure1) {
          case "ОШИБКА!" => 42
          case _ => -1
        }
      }

      println {
        monadErrorE.ensure[Int](success1)("Номер очень маленький")(_ > 1000)
      }

      println {
        monadErrorE.handleErrorWith[Int](
          monadErrorE.ensure[Int](success1)("Номер очень маленький")(_ > 1000)
        ) {
          case err => monadErrorE.raiseError(s"Вот такая ошибка: $err")
        }
      }

      println("----------------")

      val sucesss5: ErrorOr[Int] = 5000.pure[ErrorOr]
      val failure5: ErrorOr[Int] = "ОШИБКА!".raiseError[ErrorOr, Int]

      println {
        sucesss5.ensure("Номер очень маленький")(_ > 1000)
      }

      println {
        failure5.ensure("Номер очень маленький")(_ > 1000)
      }

      println("----------------")

      type ErrorTr[A] = Try[A]
      val monadErrorT: MonadError[ErrorTr, Throwable] = MonadError[ErrorTr, Throwable]

      val exn: Throwable = new RuntimeException("Пользовательское исключение!")

      println {
        exn.raiseError[ErrorTr, Int]
      }

      println {
        45.pure[ErrorTr]
      }

      println("----------------")

      type ErrorFu[A] = Future[A]
      val monadErrorF: MonadError[ErrorFu, Throwable] = MonadError[ErrorFu, Throwable]

      val future1 = exn.raiseError[ErrorFu, Int]

      future1.onComplete {
        case Failure(exc) => println(exc.getMessage)
        case Success(value) => println(value)
      }

      val future2 = 100.pure[ErrorFu]

      future2.onComplete {
        case Failure(exc) => println(exc.getMessage)
        case Success(value) => println(value)
      }


    }
  }

  //part1.part1
  //part3.part3
  //part4.part4
  //part5.part5
  //part6.part6
  part7.part7

}
