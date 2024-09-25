


import cats._

import scala.annotation.tailrec
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext, ExecutionContextExecutor, Future}
import scala.language.postfixOps
//import cats.implicits._
import cats.instances.all._
import cats.syntax.all._


object Main extends App {
  implicit val exc: ExecutionContextExecutor = scala.concurrent.ExecutionContext.global
  println("Изучаем Cats!".toUpperCase)

  private object block_1 {
    def block_1(): Unit = {
      val list = List(1, 2, 3).
        map(n => n + 1).
        map(n => n * 2).
        map(n => s"$n!")

      println(list)

      val future: Future[String] =
        Future(123).
          map(n => n + 1).
          map(n => n * 2).
          map(n => s"$n!")

      println(Await.result(future, 1 second))

      val func1: Int => Double =
        (x: Int) => x.toDouble

      val func2: Double => Double =
        (y: Double) => y * 2


      println(func1.map(func2)(1)) // composition using map
      println(func1.andThen(func2)(1)) // composition using andThen
      // res3: Double = 2.0
      println(func2(func1(1))) // composition written out by hand
      // res4: Double = 2.0


      ////////////////////////////
      val list1 = List(1, 2, 3)
      // list1: List[Int] = List(1, 2, 3)
      val list2: List[Int] = Functor[List].map(list1)(_ * 2)
      // list2: List[Int] = List(2, 4, 6)
      println(list2)

      val option1 = Option(123)
      // option1: Option[Int] = Some(value = 123)
      val option2: Option[String] = Functor[Option].map(option1)("!" + _.toString + "!")
      // option2: Option[String] = Some(value = "123")
      println(option2)
      //////////////////////////////////

      val func: Int => Int = (x: Int) => x + 1
      val func22: Int => String = (x: Int) => "#" + x.toString + "#"
      val func33: Int => String = func andThen func22
      // func: Function1[Int, Int] = repl.MdocSession$MdocApp0$$$Lambda/0x00007f1ef70ec5e0@7187a0d8

      val liftedFunc: Option[Int] => Option[String] = Functor[Option].lift(func33)
      // liftedFunc: Function1[Option[Int], Option[Int]] = cats.Functor$$Lambda/0x00007f1ef70e0000@3a68796f

      println(liftedFunc(Option(1)))

      println(Functor[List].as(list1, "As"))
      //////////////////////////////////////

      val func111 = (a: Int) => a + 1
      val func222 = (a: Int) => a * 2
      val func333 = (a: Int) => s"$a!"
      val func4 = func111.map(func222).map(func333)

      println(func4(123))
      //////////////////////////
    }
  }

  private object block_2 {
    def block_2(): Unit = {

      def doMath[F[_]](start: F[Int])(implicit functor: Functor[F]): F[Int] = start.map(n => n + 1 * 2)

      println(doMath(Option(20)))
      // res4: Option[Int] = Some(value = 22)
      println(doMath(List(1, 2, 3)))
      // res5: List[Int] = List(3, 4, 5)

      /*
      implicit class FunctorOps[F[_], A](src: F[A]) {
        def map[B](func: A => B)
                  (implicit functor: Functor[F]): F[B] =
          functor.map(src)(func)
      }
    */

      println(List(1, 2, 4).map(value => value + 1))

      final case class Box[A](value: A) {
        self =>
        def map(f: A => A): Box[A] = Box(f(self.value))
      }

      val box = Box[Int](123)
      println(box.map(value => value + 1)) //ну типа функтор
      /////////////////////////////////////

      implicit val optionFunctor: Functor[Option] = new Functor[Option] {
        def map[A, B](value: Option[A])(func: A => B): Option[B] = value.map(func)
      }

      implicit def futureFunctor(implicit ec: ExecutionContext): Functor[Future] =
        new Functor[Future] {
          def map[A, B](value: Future[A])(func: A => B): Future[B] = value.map(func)
        }

      println(Functor[Option].map(Some(25))(_ * 2))

      val future1: Future[Int] = Future.successful(456)
      println(Await.result(Functor[Future].map(future1)(_ * 3), 1 second))
      ////////////////////////////////////////////////////

      sealed trait Tree[+A]
      final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
      final case class Leaf[A](value: A) extends Tree[A]

      implicit val treeFunctor: Functor[Tree] = new Functor[Tree] {
        def map[A, B](tree: Tree[A])(func: A => B): Tree[B] = tree match {
          case Branch(left, right) => Branch(map(left)(func), map(right)(func))
          case Leaf(value) => Leaf(func(value))
        }
      }

      //Branch(Leaf(10), Leaf(20)).map(_ * 2)
      //не работает - нет функтора для Branch и Leaf

      object Tree {
        def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)

        def leaf[A](value: A): Tree[A] = Leaf(value)
      }

      import Tree._

      println(leaf(100).map(_ * 2))
      println(branch(leaf(10), leaf(20)).map(_ * 2))
      val tree: Tree[String] = branch(leaf("это"), branch(branch(leaf("редиска"), leaf("нехороший")), leaf("человек"))).map(_.toUpperCase)
      println(tree)
    }
  }

  block_1.block_1()
  block_2.block_2()

}
