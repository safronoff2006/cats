import java.sql.Timestamp
import java.time.Instant
import java.time.LocalDateTime
import java.time.ZoneOffset
import java.time.temporal.ChronoUnit._
import scala.annotation.unused
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util._

import cats.Id
import cats.Monad
import cats.MonadError
import cats.effect.IO
import cats.implicits._

object DateTimePoliform {

  trait ConvertDT[A] {
    def getMillis(value: A): Long

    def addMillis(value: A, ms: Long): A

    def compare(value: A, arg: A): Int
  }

  private object ConvertDT {
    // смарт конструктор
    private def from[A](
        fGetMillis: A => Long,
        fAddMillis: (A, Long) => A,
        fCompare: (A, A) => Int
    ): ConvertDT[A] = new ConvertDT[A] {
      override def getMillis(value: A): Long = fGetMillis(value)

      override def addMillis(value: A, ms: Long): A = fAddMillis(value, ms)

      override def compare(value: A, arg: A): Int = fCompare(value, arg)
    }

    // имплементации для конкретных типов
    implicit val timestampConvertDT: ConvertDT[Timestamp] = from[Timestamp](
      _.getTime,
      (a, ms) => new Timestamp(a.getTime + ms),
      (a, b) => a compareTo b
    )
    implicit val longConvertDT: ConvertDT[Long] = from[Long](
      x => x,
      (a, ms) => a + ms,
      (a, b) => if (a < b) -1 else if (a > b) 1 else 0
    )
    @unused
    implicit val durationConvertDT: ConvertDT[Duration] = from[Duration](
      _.toMillis,
      (a, ms) => Duration.apply(a.toMillis + ms, MILLISECONDS),
      (a, b) => a compareTo b
    )
    implicit val finitedurationConvertDT: ConvertDT[FiniteDuration] = from[FiniteDuration](
      _.toMillis,
      (a, ms) => Duration.apply(a.toMillis + ms, MILLISECONDS),
      (a, b) => a compareTo b
    )
    implicit val localDateTimeConvertDT: ConvertDT[LocalDateTime] = from[LocalDateTime](
      _.atOffset(ZoneOffset.UTC).toInstant.toEpochMilli,
      (a, ms) => a.plus(ms, MILLIS),
      (a, b) => a compareTo b
    )

    // суммонер
    def apply[T](implicit ev: ConvertDT[T]): ConvertDT[T] = ev
  }

  // имплисит классы
  implicit class TimestampExtensions[F[_]: Monad, A: ConvertDT](ts: F[A]) {
    def getMillis: F[Long] = for {
      t <- ts
    } yield ConvertDT[A].getMillis(t)

    def ++(dur: Duration): F[A] = for {
      t <- ts
    } yield ConvertDT[A].addMillis(t, dur.toMillis)

    def --(dur: Duration): F[A] = for {
      t <- ts
    } yield ConvertDT[A].addMillis(t, -1 * dur.toMillis)

    def --(ts2: F[A]): F[Duration] = for {
      t  <- ts
      t2 <- ts2
    } yield Duration.apply(ConvertDT[A].getMillis(t) - ConvertDT[A].getMillis(t2), MILLISECONDS)

    def >(arg: F[A]): F[Boolean] = for {
      t <- ts
      a <- arg
    } yield ConvertDT[A].compare(t, a) > 0

    def >=(arg: F[A]): F[Boolean] = for {
      t <- ts
      a <- arg
    } yield ConvertDT[A].compare(t, a) >= 0

    def <(arg: F[A]): F[Boolean] = for {
      t <- ts
      a <- arg
    } yield ConvertDT[A].compare(t, a) < 0

    def <=(arg: F[A]): F[Boolean] = for {
      t <- ts
      a <- arg
    } yield ConvertDT[A].compare(t, a) <= 0

    def ===(arg: F[A]): F[Boolean] = for {
      t <- ts
      a <- arg
    } yield ConvertDT[A].compare(t, a) == 0

    def =!=(arg: F[A]): F[Boolean] = for {
      t <- ts
      a <- arg
    } yield ConvertDT[A].compare(t, a) != 0
  }

  implicit class TimestampExtensionsId[A: ConvertDT](ts: Id[A]) {
    def getMillis: Id[Long] = TimestampExtensions[Id, A](ts) getMillis

    def ++(dur: Duration): Id[A] = TimestampExtensions[Id, A](ts) ++ dur

    def --(dur: Duration): Id[A] = TimestampExtensions[Id, A](ts) -- dur

    def --(ts2: Id[A]): Id[Duration] = TimestampExtensions[Id, A](ts) -- ts2

    def >(arg: Id[A]): Id[Boolean] = TimestampExtensions[Id, A](ts) > arg

    def >=(arg: Id[A]): Id[Boolean] = TimestampExtensions[Id, A](ts) >= arg

    def <(arg: Id[A]): Id[Boolean] = TimestampExtensions[Id, A](ts) < arg

    def <=(arg: Id[A]): Id[Boolean] = TimestampExtensions[Id, A](ts) <= arg

    def ===(arg: Id[A]): Id[Boolean] = TimestampExtensions[Id, A](ts) === arg

    def =!=(arg: Id[A]): Id[Boolean] = TimestampExtensions[Id, A](ts) =!= arg
  }

}

object Main extends App {

  import DateTimePoliform._

  println("Демо\n")

  println("----------------------------------------------------------")
  println("|             Полиморфизм по типу контекста              |")
  println("----------------------------------------------------------\n")

  // Незавернуто (контекст Id)
  private val t1_1: Timestamp = Timestamp.from(Instant.now())
  private val t1_2: Timestamp = t1_1 ++ 10.seconds
  println(t1_1, t1_2) // (2025-05-30 16:09:36.391689555,2025-05-30 16:09:46.391)

  private val t1_3 = Timestamp.from(Instant.now())
  private val t1_4 = t1_3 -- 10.seconds
  println(t1_3, t1_4) // (2025-05-30 16:09:36.631647228,2025-05-30 16:09:26.631)

  private val d1_5 = t1_2 -- t1_1
  println(d1_5) // 10000 milliseconds

  println(t1_1.getMillis) // 1748610754289

  println(t1_2 > t1_1)   // true
  println(t1_2 >= t1_1)  // true
  println(t1_2 < t1_1)   // false
  println(t1_2 <= t1_1)  // false
  println(t1_2 === t1_1) // false
  println(t1_2 =!= t1_1) // true
  println(t1_1 === t1_1) // true
  println()

  // Контекст - Option
  private val ot1_1: Option[Timestamp] = Timestamp.from(Instant.now()).some
  private val ot1_2: Option[Timestamp] = ot1_1 ++ 10.seconds
  println(ot1_1, ot1_2) // (Some(2025-05-30 16:29:56.152390691),Some(2025-05-30 16:30:06.152))

  private val ot1_3 = ot1_1 -- 10.seconds
  private val ot1_4 = Option.empty[Timestamp] -- 10.seconds
  println(ot1_3, ot1_4)   // (Some(2025-05-30 16:32:53.595),None)
  println(ot1_2 -- ot1_1) // Some(10000 milliseconds)

  println(ot1_2 > ot1_1)   // Some(true)
  println(ot1_2 >= ot1_1)  // Some(true)
  println(ot1_2 < ot1_1)   // Some(false)
  println(ot1_2 <= ot1_1)  // Some(false)
  println(ot1_2 === ot1_1) // Some(false)
  println(ot1_2 =!= ot1_1) // Some(true)
  println(ot1_1 === ot1_1) // Some(true)
  println()

  // Контекст - Either
  private val et1_1: Either[String, Timestamp] = Timestamp.from(Instant.now()).asRight[String]
  private val et1_2                            = et1_1 ++ 10.seconds
  println(et1_1, et1_2)   // (Right(2025-05-30 16:34:15.587920476),Right(2025-05-30 16:34:25.587))
  println(et1_2 -- et1_1) // Right(10000 milliseconds)
  println(et1_2 > et1_1)  // Right(true)
  println()

  // используя MonadError (из Cats)
  private type ErrorOr[A] = Either[String, A]
  private val monadError: MonadError[ErrorOr, String] = MonadError[ErrorOr, String]

  private val met1_1: ErrorOr[Timestamp] = et1_1 match {
    case Left(err)    => monadError.raiseError(err)
    case Right(value) => value.pure[ErrorOr]
  }
  private val met1_2: ErrorOr[Timestamp] = met1_1 ++ 10.seconds
  println(met1_1, met1_2) // (Right(2025-05-30 17:11:13.877627638),Right(2025-05-30 17:11:23.877))
  println()

  // Контекст - Try
  private val tt1_1: Try[Timestamp] = Try(Timestamp.from(Instant.now()))
  private val tt1_2: Try[Timestamp] = tt1_1 ++ 10.seconds
  println(tt1_1, tt1_2)   // (Success(2025-05-30 17:12:45.777369399),Success(2025-05-30 17:12:55.777)
  println(tt1_2 -- tt1_1) // Success(10000 milliseconds)
  println(tt1_2 > tt1_1)  // Success(true)
  println()

  // Контекст - Future
  implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global

  private val ft1_1                    = Future(Timestamp.from(Instant.now()))
  private val ft1_2: Future[Timestamp] = ft1_1 ++ 10.seconds
  private val ft1_3: Future[(Timestamp, Timestamp)] = for {
    r1 <- ft1_1
    r2 <- ft1_2
  } yield r1 -> r2

  ft1_3.onComplete {
    case Success(value) => println(s"Success $value")
    case Failure(t)     => println(s"Failure ${t.getMessage} ")
  } // Success (2025-05-30 17:31:40.649501762,2025-05-30 17:31:50.649)

  Thread.sleep(10)
  println()

  // Контекст - IO монада из CatsEffect
  import cats.effect.unsafe.implicits.global

  private val e1_1 = IO(Timestamp.from(Instant.now()))
  private val e1_2 = e1_1 ++ 10.seconds

  private val e1_3 = for {
    r1 <- e1_1
    r2 <- e1_2
    _  <- IO.println(s"Effect result $r1 $r2")

  } yield () // Effect result 2025-05-30 18:59:09.78247636 2025-05-30 18:59:19.783
  e1_3.unsafeRunSync()
  println()

  println("---------------------------------------------------")
  println("|           Полиморфизм по типу данных            |")
  println("---------------------------------------------------\n")

  println(9999999999999L -- 9999999999990L) // 9 milliseconds
  println(9999999999999L ++ 100.millis)     // 10000000000099

  private val t2_1            = Timestamp.from(Instant.now)
  private val t2_2: Timestamp = t2_1 ++ 1.seconds
  println(t2_1, t2_2)   // (2025-05-30 20:17:09.625132323,2025-05-30 20:17:10.625)
  println(t2_2 -- t2_1) // 1000 milliseconds

  private val d2_3 = 10.seconds
  private val d2_4 = d2_3 -- 10.millis
  println(d2_3, d2_4) // (10 seconds,9990 milliseconds)

  private val ldt2_5                = LocalDateTime.now
  private val ldt2_6: LocalDateTime = ldt2_5 ++ 10.seconds
  println(ldt2_5, ldt2_6)   // (2025-05-30T20:25:33.461648075,2025-05-30T20:25:43.461648075)
  println(ldt2_6 -- ldt2_5) // 10000 milliseconds

  println(9999999999999L.some -- 9999999999990L.some) // Some(9 milliseconds)
  println(9999999999999L.some ++ 100.millis)          // Some(10000000000099)

  private val ot2_1 = Timestamp.from(Instant.now).some
  private val ot2_2 = ot2_1 ++ 1.seconds
  println(ot2_1, ot2_2) // (Some(2025-05-30 20:30:59.81685418),Some(2025-05-30 20:31:00.816))

  private val od2_3 = 10.seconds.some
  private val od2_4 = od2_3 -- 10.millis.some
  println(od2_3, od2_4) // (Some(10 seconds),Some(9990 milliseconds))

  private val oldt2_5 = LocalDateTime.now.some
  private val oldt2_6 = oldt2_5 ++ 10.seconds
  println(oldt2_5, oldt2_6)   // (Some(2025-05-30T20:37:10.462179725),Some(2025-05-30T20:37:20.462179725))
  println(oldt2_6 -- oldt2_5) // Some(10000 milliseconds)

}
