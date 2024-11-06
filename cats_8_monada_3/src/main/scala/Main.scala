


import cats._
import cats.data.{Writer, WriterT}
import cats.implicits._
import cats.instances.all._
import cats.syntax.all._

import scala.annotation.tailrec
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.DurationInt
import scala.language.postfixOps
import scala.util.Random


object Main extends App {

  println("Изучаем Cats!".toUpperCase)

  object part1 {
    def part1: Unit = {

      // import cats.instances.vector._ // for Monoid

      // type Writer[W, A] = WriterT[Id, W, A]
      // в Cats тип Writer реализован через монадный трансформер WriterT

      val w: WriterT[Id, Vector[String], Int] = Writer(Vector(
        "It was the best of times",
        "it was the worst of times"
      ), 1859)

      println(w)

      //Создаем Writer из результата

      // import cats.instances.vector._   // for Monoid
      import cats.syntax.applicative._ // for pure

      type Logged[A] = Writer[Vector[String], A]
      val logged1: Logged[Int] = 123.pure[Logged]
      println(logged1)

      //Создаем Writer из журнала
      import cats.syntax.writer._ // for tell

      val logged2: Writer[Vector[String], Unit] = Vector("msg1", "msg2", "msg3").tell
      println(logged2)

      //Создаем Writer из результата и журнала

      //1. используем Writer.apply
      val a: WriterT[Id, Vector[String], Int] = Writer(Vector("msg1", "msg2", "msg3"), 123)

      //2. используем синтаксис  cats.syntax.writer
      // import cats.syntax.writer._ // for writer
      val b: Writer[Vector[String], Int] = 123.writer(Vector("msg1", "msg2", "msg3"))

      val aResult: Int = a.value
      val aLog: Vector[String] = a.written
      println(aResult)
      println(aLog)

      val (log, result) = b.run
      println(log, result)

    }
  }

  object part2 {
    def part2: Unit = {

      import cats.syntax.applicative._ // for pure
      import cats.syntax.writer._ // for tell

      //Журнал в a Writer сохраняется, когда мы map или flatMap над ним.
      // flatMap добавляет журналы из источника Writer и результат функции секвенирования пользователя.
      // По этой причине хорошей практикой является использование типа журнала,
      // который имеет эффективные операции добавления и конкатенации, такие как Vector

      //flatMap и map на Writer
      type Logged[A] = Writer[Vector[String], A]
      val writer1: WriterT[Id, Vector[String], Int] = for {
        a <- 10.pure[Logged]
        _ <- Vector("a", "b", "c").tell
        b <- 32.writer(Vector("x", "y", "z"))
      } yield a + b

      println(writer1.run)

      //map над журналом
      val writer2 = writer1.mapWritten(_.map(_.toUpperCase))
      println(writer2.run)

      //Мы можем преобразовать как журнал, так и результат одновременно,
      // используя bimap или mapBoth. bimap принимает два параметра функции, один для журнала и один для результата.
      // mapBoth принимает одну функцию, которая принимает два параметра:

      val writer3 = writer1.bimap(
        log => log.map(_.toUpperCase),
        res => res * 100
      )
      println(writer3.run)

      val writer4 = writer1.mapBoth { (log, res) =>
        (log.map(_ + "!"), res * 1000)
      }
      println(writer4.run)

      //очистка журнала
      val writer5 = writer1.reset
      println(writer5.run)

      //поменять местами журнал и результат
      val writer6: WriterT[Id, Int, Vector[String]] = writer1.swap
      println(writer6.run)

    }
  }

  object part3 {
    def part3 = {

      def slowly[A](body: => A): A =
        try body finally Thread.sleep(100)


      def factorial(n: Int): Int = {
        val ans = slowly(if (n == 0) 1 else n * factorial(n - 1))
        println(s"fact $n $ans")
        ans
      }

      //factorial(5)

      // Если мы начнем несколько факториалов параллельно,
      // сообщения журнала могут перемежаться на стандартном выходе.
      // Это затрудняет просмотр того, какие сообщения приходят из какого вычисления:

      import scala.concurrent.ExecutionContext.Implicits._

      /*
            Await.result(Future.sequence(Vector(
              Future(factorial(5)),
              Future(factorial(5))
            )), 15.seconds)
      */

      // Перепишите factorial так, чтобы он захватывал сообщения журнала в Writer.
      // Продемонстрируйте, что это позволяет нам надежно разделять журналы для параллельных вычислений.

      import cats.syntax.applicative._ // for pure
      import cats.syntax.writer._ // for tell

      type Logged[A] = Writer[Vector[String], A]
      42.pure[Logged]
      Vector("Message").tell
      41.pure[Logged].map(_ + 1)


      def factorialW(n: Int): Logged[Int] =
        for {
          ans <- if (n == 0) {
            1.pure[Logged]
          } else {
            slowly(factorialW(n - 1).map(_ * n))
          }
          _ <- Vector(s"fact $n $ans").tell
        } yield ans

      val (log, res) = factorialW(5).run
      // println(log, res)

      // Мы можем запустить несколько из factorials параллельно следующим образом,
      // собирая их журналы независимо, не опасаясь перемежения:

      val result: Vector[Vector[String]] = Await.result(
        Future.sequence(Vector(
          Future(factorialW(5)),
          Future(factorialW(5))
        )).map(_.map(_.written))
        ,
        5 seconds
      )

      println(result)

    }
  }

  object part4 {
    def part4 = {

      def slowlyR[A](body: => A): (A, Int) = {
        val rnd = Random.between(1, 100)
        Thread.sleep(rnd)
        body -> rnd
      }

      import scala.concurrent.ExecutionContext.Implicits._

      import cats.syntax.applicative._ // for pure
      import cats.syntax.writer._ // for tell

      type Logged[A] = Writer[Vector[String], A]

      def factorialWR(n: Int): Logged[Int] =
        for {
          ans <- if (n == 0) {
            (1 -> 0).pure[Logged]
          } else {
            val sl: (WriterT[Id, Vector[String], Int], Int) = slowlyR(factorialWR(n - 1).map(_ * n))
            sl._1.map( _ -> sl._2 )
          }
          _ <- Vector(s"fact $n val=${ans._1} dur=${ans._2}").tell
        } yield ans._1

      val result1: Vector[Vector[String]] = Await.result(
        Future.sequence(Vector(
          Future(factorialWR(5)),
          Future(factorialWR(6))
        )).map(_.map(_.written))
        , 5 seconds
      )

      println(result1)

      val result2: Vector[(Vector[String], Int)] =  Await.result(
        Future.sequence(Vector(
          Future(factorialWR(5)),
          Future(factorialWR(6))
        )).map(_.map(_.run))
        , 5 seconds
      )

      println(result2)

    }
  }

  //part1.part1
  //part2.part2
  //part3.part3
  part4.part4

}
