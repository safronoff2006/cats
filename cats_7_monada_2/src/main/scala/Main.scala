


import cats._
import cats.implicits._
import cats.instances.all._
import cats.syntax.all._

import scala.annotation.tailrec
import scala.language.postfixOps


object Main extends App {

  println("Изучаем Cats!".toUpperCase)

  object part1 {
    def part1: Unit = {
      val x = {
        println("Computing X")
        math.random()
      }

      println("----------")
      println(x)
      println(x)

      def y: Double = {
        println("Computing Y")
        math.random()
      }

      println("----------")
      println(y)
      println(y)


      lazy val z = {
        println("Computing Z")
        math.random()
      }

      println("----------")
      println(z)
      println(z)
    }
  }

  object part2 {
    def part2: Unit = {
      val now = Eval.now {
        println("Computing now")
        math.random() + 1000
      }

      val always = Eval.always {
        println("Computing always")
        math.random() + 3000
      }

      val later = Eval.later {
        println("Computing later")
        math.random() + 2000
      }

      println("----------")
      println(now.value)
      println("----------")
      println(always.value)
      println("----------")
      println(later.value)

      println("===================")

      println("----------")
      println(now.value)
      println("----------")
      println(always.value)
      println("----------")
      println(later.value)


    }
  }

  object part3 {
    def part3 = {

      val greeting = Eval
        .always{ println("Step 1"); "Hello" }
        .map{ str => println("Step 2"); s"$str world" }

      println {
        greeting.value
      }

      println("========================================")
      val ans = for {
        a <- Eval.now{ println("Calculating A"); 40 }
        b <- Eval.always{ println("Calculating B"); 2 }
      } yield {
        println("Adding A and B")
        a + b
      }

      println("----- first access ------")
      println(ans.value)
      println("----- second access ------")
      println(ans.value)

      println("========================================")

      val saying = Eval
        .always{ println("Step 1"); "The cat" }
        .map{ str => println("Step 2"); s"$str sat on" }
        .memoize
        .map{ str => println("Step 3"); s"$str the mat" }

      println("----- first access ------")
      println(saying.value)
      println("----- second access ------")
      println(saying.value)
      println("----- third access ------")
      println(saying.value)



      def factorial(n: BigInt): BigInt =
        if(n == 1) n else n * factorial(n - 1)

      //println(factorial(50000))  //переполнение стэка

      def factorialE(n: BigInt): Eval[BigInt] =
        if(n == 1) {
          Eval.now(n)
        } else {
          Eval.defer(factorialE(n - 1).map(_ * n))
        }

      println(factorialE(50000).value)

    }
  }

  object part4 {
    def part4 = {
      def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B = as match {
          case head :: tail =>
            fn(head, foldRight(tail, acc)(fn))
          case Nil =>
            acc
        }

      val list1 = List(1,2,3,4)
      println(foldRight(list1, 0)(_ + _))
    }
  }

  object part5 {
    def part5 = {

      def foldRightEval[A, B](as: List[A], acc: Eval[B])(fn: (A, Eval[B]) => Eval[B]): Eval[B] =
        as match {
          case head :: tail =>
            Eval.defer(fn(head, foldRightEval(tail, acc)(fn)))
          case Nil =>
            acc
        }

      def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B =
        foldRightEval(as, Eval.now(acc)) { (a, b) =>
          b.map(fn(a, _))
        }.value

      println(foldRight((1 to 100000).toList, 0L)(_ + _))

    }
  }

  part1.part1
  part2.part2
  part3.part3
  part4.part4
  part5.part5



}
