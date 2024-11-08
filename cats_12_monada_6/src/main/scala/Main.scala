



import cats._
import cats.implicits._

import scala.annotation.tailrec
import scala.language.postfixOps


object Main extends App {

  println("Изучаем Cats!".toUpperCase)

  object part1 {
    def part1: Unit = {
      // Определение пользовательских монад

      // Мы можем определить Monad для пользовательского типа, предоставив реализации трех методов:
      // flatMap, pure, и метод, который мы еще не видели, называемый tailRecM.
      // Вот реализация для Monad Option в качестве примера:



      val optionMonad = new Monad[Option] {
        def flatMap[A, B](opt: Option[A])
                         (fn: A => Option[B]): Option[B] =
          opt flatMap fn

        def pure[A](opt: A): Option[A] =
          Some(opt)

        @tailrec
        def tailRecM[A, B](a: A)
                          (fn: A => Option[Either[A, B]]): Option[B] =
          fn(a) match {
            case None           => None
            case Some(Left(a1)) => tailRecM(a1)(fn)
            case Some(Right(b)) => Some(b)
          }
      }

      // Метод tailRecM представляет собой оптимизацию,
      // используемую в Cats для ограничения объема стекового пространства,
      // потребляемого вложенными вызовами flatMap.
      // Метод взят из статьи 2015 года создателя PureScript Фила Фримена.
      // Метод должен рекурсивно вызывать себя, пока результат fn не вернет Right.

      // Чтобы мотивировать его использование, давайте используем следующий пример:
      // Предположим, мы хотим написать метод, который вызывает функцию, пока функция не укажет, что она должна остановиться.
      // Функция вернет экземпляр монады, потому что, как мы знаем, монады представляют последовательность,
      // и многие монады имеют некоторое представление об остановке.

      // Мы можем записать этот метод в терминах flatMap.

      import cats.syntax.flatMap._ // For flatMap
      import cats.instances.option._


      def retry[F[_]: Monad, A](start: A)(f: A => F[A]): F[A] =
        f(start).flatMap{ a =>
          retry(a)(f)
        }

      // К сожалению, это не безопасно для стека. Это работает для небольших входных данных.

      retry(100)(a => if(a == 0) Option.empty else Some(a - 1))

      //retry(100000)(a => if(a == 0) Option.empty else Some(a - 1))
      // не работает - переполнение стека

      import cats.syntax.functor._ // for map

      def retryTailRecM[F[_]: Monad, A](start: A)(f: A => F[A]): F[A] =
        Monad[F].tailRecM(start){ a =>
          f(a).map(a2 => Left(a2))
        }

      retryTailRecM(100000)(a => if(a == 0) Option.empty else Some(a - 1))

      // Важно отметить, что нам нужно явно вызвать tailRecM.
      // Не существует преобразования кода, которое преобразует нехвостовой рекурсивный код в хвостовой рекурсивный код,
      // использующий tailRecM. Однако есть несколько утилит, предоставляемых классом типа Monad,
      // которые упрощают написание таких методов.
      // Например, мы можем переписать retry в терминах iterateWhileM и нам не нужно явно вызывать tailRecM.

      import cats.syntax.monad._ // for iterateWhileM

      def retryM[F[_]: Monad, A](start: A)(f: A => F[A]): F[A] =
        start.iterateWhileM(f)(_ => true)

      retryM(100000)(a => if (a == 0) Option.empty else Some(a - 1))

      // Мы рассмотрим больше методов, которые используются, tailRecM в разделе 7.1.

      // Все встроенные монады в Cats имеют реализации с хвостовой рекурсией tailRecM,
      // хотя написание таковой для пользовательских монад может оказаться сложной задачей… как мы увидим.

    }
  }

  object part2 {
    def part2 = {
      // Упражнение: Дальнейшее разветвление с помощью монад
      // Давайте напишем Monad для нашего Tree типа данных из прошлой главы. Вот тип еще раз:

      sealed trait Tree[+A]

      final case class Branch[A](left: Tree[A], right: Tree[A])
        extends Tree[A]

      final case class Leaf[A](value: A) extends Tree[A]

      def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
        Branch(left, right)

      def leaf[A](value: A): Tree[A] =
        Leaf(value)

      /*
      implicit val treeFunctor: Functor[Tree] = new Functor[Tree] {
        def map[A, B](tree: Tree[A])(func: A => B): Tree[B] = tree match {
          case Branch(left, right) => Branch(map(left)(func), map(right)(func))
          case Leaf(value) => Leaf(func(value))
        }
      }


      println(leaf(100).map(_ * 2))
      println(branch(leaf(10), leaf(20)).map(_ * 2))
      val tree: Tree[String] = branch(leaf("это"), branch(branch(leaf("редиска"), leaf("нехороший")), leaf("человек"))).map(_.toUpperCase)
      println(tree)
    */

      // Убедитесь, что код работает на экземплярах Branch и Leaf, и что Monad обеспечивает Functor - подобное поведение бесплатно.
      // Также убедитесь, что наличие Monad области действия позволяет нам использовать для включений,
      // несмотря на тот факт, что мы не реализовали его напрямую flatMap или map на Tree.

      // Не думайте, что вам нужно сделать tailRecM хвостовую рекурсию.
      // Сделать это довольно сложно.
      // Мы включили в решения как хвостовую рекурсию, так и не хвостовую рекурсию, чтобы вы могли проверить свою работу.

      // Код для flatMap похож на код для map.
      // Опять же, мы рекурсивно проходим вниз по структуре и используем результаты для func построения нового Tree.

      // Код для tailRecM довольно сложен, независимо от того, делаем ли мы его с хвостовой рекурсией или нет.

      // Если следовать типам, то получается решение без хвостовой рекурсии:


      implicit val treeMonad = new Monad[Tree] {
        def pure[A](value: A): Tree[A] = Leaf(value)

        def flatMap[A, B](tree: Tree[A])(func: A => Tree[B]): Tree[B] =
          tree match {
            case Branch(l, r) =>
              Branch(flatMap(l)(func), flatMap(r)(func))
            case Leaf(value) =>
              func(value)
          }

        def tailRecM[A, B](a: A)(func: A => Tree[Either[A, B]]): Tree[B] =
          flatMap(func(a)) {
            case Left(value) =>
              tailRecM(value)(func)
            case Right(value) =>
              Leaf(value)
          }
      }

      println(leaf(100).map(_ * 2))
      println(branch(leaf(10), leaf(20)).map(_ * 2))
      val tree: Tree[String] = branch(leaf("это"), branch(branch(leaf("редиска"), leaf("нехороший")), leaf("человек"))).map(_.toUpperCase)
      println(tree)

      // Решение выше отлично подходит для этого упражнения.
      // Его единственный недостаток в том, что Cats не может гарантировать безопасность стека.

      // Независимо от того, какую версию tailRecM мы определяем,
      // мы можем использовать наши Monad to flatMap и map on Trees:

      println("---------------------")

      val tree1: Tree[Int] = branch(leaf(100), leaf(200)).
        flatMap(x => branch(leaf(x - 1), leaf(x + 1)))

      println {
        tree1
      }

      // Мы также можем преобразовать, Trees используя for comprehensions:

      val tree2: Tree[Int] =  for {
        a <- branch(leaf(100), leaf(200))
        b <- branch(leaf(a - 10), leaf(a + 10))
        c <- branch(leaf(b - 1), leaf(b + 1))
      } yield c

      println {
        tree2
      }


      // Стэкобезопасное решение в part 3

      println("---------------------")
    }
  }

  object part3 {
    def part3 = {

      // Решение с рекурсией хвоста написать гораздо сложнее.
      // Мы адаптировали это решение из этого поста Stack Overflow Назария Бардюка.
      // Оно включает явный обход дерева в глубину, поддержание списка open узлов для посещения
      // и closed списка узлов для использования при реконструкции дерева:

      sealed trait Tree[+A]

      final case class Branch[A](left: Tree[A], right: Tree[A])
        extends Tree[A]

      final case class Leaf[A](value: A) extends Tree[A]

      def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
        Branch(left, right)

      def leaf[A](value: A): Tree[A] =
        Leaf(value)

      implicit val treeMonad = new Monad[Tree] {
        def pure[A](value: A): Tree[A] = Leaf(value)

        def flatMap[A, B](tree: Tree[A])(func: A => Tree[B]): Tree[B] =
          tree match {
            case Branch(l, r) =>
              Branch(flatMap(l)(func), flatMap(r)(func))
            case Leaf(value)  =>
              func(value)
          }

        def tailRecM[A, B](arg: A)(func: A => Tree[Either[A, B]]): Tree[B] = {

            @tailrec
            def loop( open: List[Tree[Either[A, B]]], closed: List[Option[Tree[B]]]): List[Tree[B]] =
              open match {
                case Branch(l, r) :: next =>
                  loop(l :: r :: next, None :: closed)

                case Leaf(Left(value)) :: next =>
                  loop(func(value) :: next, closed)

                case Leaf(Right(value)) :: next =>
                  loop(next, Some(pure(value)) :: closed)

                case Nil =>
                  closed.foldLeft(Nil: List[Tree[B]]) { (acc, maybeTree) =>
                    maybeTree.map(_ :: acc).getOrElse {
                      val left :: right :: tail = acc
                      branch(left, right) :: tail
                    }
                  }
              }

            loop(List(func(arg)), Nil).head

        }

      }

      // Независимо от того, какую версию tailRecM мы определяем,
      // мы можем использовать наши Monad to flatMap и map on Trees:

      val tree1: Tree[Int] = branch(leaf(100), leaf(200)).
        flatMap(x => branch(leaf(x - 1), leaf(x + 1)))

      println {
        tree1
      }

      // Мы также можем преобразовать, Trees используя for comprehensions:

     val tree2: Tree[Int] =  for {
        a <- branch(leaf(100), leaf(200))
        b <- branch(leaf(a - 10), leaf(a + 10))
        c <- branch(leaf(b - 1), leaf(b + 1))
      } yield c

      println {
        tree2
      }

      // Монада для Option обеспечивает семантику fail-fast.
      // Монада для List обеспечивает семантику конкатенации.
      // Какова семантика flatMap для бинарного дерева?
      // Каждый узел в дереве может быть потенциально заменен целым поддеревом,
      // что создает своего рода «растущее» или «распускающееся» поведение, напоминающее конкатенацию списков по двум осям.

      println("---------------------")

    }
  }

  part1.part1
  part2.part2
  part3.part3


}

//В этой главе мы рассмотрели монады вблизи.

// Мы увидели, что flatMap можно рассматривать как оператор для упорядочивания вычислений, диктующий порядок,
// в котором должны выполняться операции.
// С этой точки зрения, Option представляет вычисление, которое может завершиться неудачей без сообщения об ошибке,
// Either представляет вычисления, которые могут завершиться неудачей с сообщением,
// List представляет несколько возможных результатов и
// Future представляет вычисление, которое может произвести значение в какой-то момент в будущем.

// Мы также рассмотрели некоторые пользовательские типы и структуры данных,
// которые предоставляет Cats, включая
// Id, Reader, Writer, и State.
// Они охватывают широкий спектр вариантов использования.

// Наконец, в маловероятном случае, если нам придется реализовать собственную монаду,
// мы узнали об определении собственного экземпляра с помощью tailRecM.
// tailRecM — это странная загвоздка, которая является уступкой построению функциональной библиотеки программирования,
// которая по умолчанию является стекобезопасной.
// Нам не нужно понимать, tailRecM чтобы понимать монады, но наличие ее дает нам преимущества,
// за которые мы можем быть благодарны при написании монадического кода.

