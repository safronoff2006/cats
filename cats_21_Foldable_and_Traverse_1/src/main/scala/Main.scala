



import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.language.postfixOps


object Main extends App {

  println("Изучаем Cats!".toUpperCase)
  println("Foldable and Traverse")

  object part1 {

    // Foldable and Traverse

    // В этой главе мы рассмотрим два класса типов, которые охватывают итерацию по коллекциям:

    // - Foldable абстрагирует знакомые foldLeft и foldRight операции;
    // - Traverse — это абстракция более высокого уровня, которая позволяет Applicative
    // выполнять итерации с меньшими трудностями, чем сворачивание.

    // Начнем с рассмотрения Foldable, а затем рассмотрим случаи, когда складывание становится сложным и Traverse удобнее.

    def chapter1 = {

    // Foldable

      // Тайп класс Foldable охватывает foldLeft и foldRight методы, которые мы используем в последовательностях,
      // таких как Lists, Vectors и Streams.
      // Используя Foldable, мы можем писать универсальные свертки, которые работают с различными типами последовательностей.
      // Мы также можем изобретать новые последовательности и вставлять их в наш код.
      // Foldable дает нам отличные варианты использования для Monoids и монаде Eval.

      // Folds and Folding

      // Давайте начнем с краткого обзора общей концепции свертывания.
      // Мы предоставляем значение аккумулятора и бинарную функцию для его объединения с каждым элементом в последовательности:

      def show[A](list: List[A]): String =
        list.foldLeft("nil")((accum, item) => s"$item then $accum")

      println {
        show(Nil)
        // res0: String = "nil"
      }
      println {
        show(List(1, 2, 3))
        // res1: String = "3 then 2 then 1 then nil"
      }

      // Метод foldLeft работает рекурсивно вниз по последовательности.
      // Наша бинарная функция вызывается повторно для каждого элемента,
      // результат каждого вызова становится аккумулятором для следующего.
      // Когда мы достигаем конца последовательности, конечный аккумулятор становится нашим конечным результатом.

      // В зависимости от выполняемой нами операции, порядок, в котором мы складываем, может быть важен.
      // Из-за этого существует два стандартных варианта сложения:

      // - foldLeft проходит «слева» «направо» (от начала до конца);
      // - foldRight проходит справа налево (от финиша к старту).

      // Рисунок ниже иллюстрирует каждое направление.
      // resources/Иллюстрация_foldLeft_и_foldRight.svg

      // foldLeft и foldRight эквивалентны, если наша бинарная операция ассоциативна.
      // Например, мы можем суммировать a List[Int], складывая в любом направлении,
      // используя 0в качестве нашего аккумулятора и сложение в качестве нашей операции:

      println(
        List(1, 2, 3).foldLeft(0)(_ + _),
        // res2: Int = 6
        List(1, 2, 3).foldRight(0)(_ + _)
        // res3: Int = 6
      )

      // Если мы предоставляем неассоциативный оператор, порядок оценки имеет значение.
      // Например, если мы складываем с помощью вычитания, мы получаем разные результаты в каждом направлении:

      println(
        List(1, 2, 3).foldLeft(0)(_ - _),
        // res4: Int = -6
        List(1, 2, 3).foldRight(0)(_ - _)
        // res5: Int = 2
      )

      println("-------")

      // Упражнение: Размышление о Folds

      // Попробуйте использовать foldLeft и foldRight с пустым списком в качестве аккумулятора
      // и :: в качестве бинарного оператора.
      // Какие результаты вы получите в каждом случае?

      // Свертка слева направо переворачивает список:
      println {
        List(1, 2, 3).foldLeft(List.empty[Int])((a, i) => i :: a)
        // res6: List[Int] = List(3, 2, 1)
      }

      // Свертка справа налево копирует список, сохраняя порядок неизменным:
      println {
        List(1, 2, 3).foldRight(List.empty[Int])((i, a) => i :: a)
        // res7: List[Int] = List(1, 2, 3)
      }

      // Обратите внимание, что нам нужно тщательно указать тип аккумулятора, чтобы избежать ошибки типа.
      // Мы используем, List.empty[Int], чтобы избежать вывода типа аккумулятора как Nil.type или List[Nothing]:

      /*
            List(1, 2, 3).foldRight(Nil)(_ :: _)
            // error: type mismatch;
            //  found   : List[Int]
            //  required: scala.collection.immutable.Nil.type
            // List(1, 2, 3).foldRight(Nil)(_ :: _)
            //
      */

      println("-------")

      // Упражнение: Строительные леса. Другие методы.

      // foldLeft и foldRight являются очень общими методами.
      // Мы можем использовать их для реализации многих других известных нам высокоуровневых операций последовательности.
      // Докажите это себе, реализовав замены для List's map, flatMap, filter, и sum методов в терминах foldRight.

      def map[A, B](list: List[A])(func: A => B): List[B] =
        list.foldRight(List.empty[B]) { (item, accum) =>
          func(item) :: accum
        }
      println {
        map(List(1, 2, 3))(_ * 2)
        // res9: List[Int] = List(2, 4, 6)
      }

      def flatMap[A, B](list: List[A])(func: A => List[B]): List[B] =
        list.foldRight(List.empty[B]) { (item, accum) =>
          func(item) ::: accum
        }

      println {
        flatMap(List(1, 2, 3))(a => List(a, a * 10, a * 100))
        // res10: List[Int] = List(1, 10, 100, 2, 20, 200, 3, 30, 300)
      }

      def filter[A](list: List[A])(func: A => Boolean): List[A] =
        list.foldRight(List.empty[A]) { (item, accum) =>
          if(func(item)) item :: accum else accum
        }

      println {
        filter(List(1, 2, 3))(_ % 2 == 1)
        // res11: List[Int] = List(1, 3)
      }

      // Мы предоставили два определения sum, одно из которых использует scala.math.Numeric
      // (что точно воссоздает встроенную функциональность)…

      //import scala.math.Numeric

      def sumWithNumeric[A](list: List[A])
                           (implicit numeric: Numeric[A]): A =
        list.foldRight(numeric.zero)(numeric.plus)


      println {
        sumWithNumeric(List(1, 2, 3))
        // res12: Int = 6
      }

      // и один с использованием cats.Monoid (что больше соответствует содержанию этой книги):

      import cats.Monoid

      def sumWithMonoid[A](list: List[A])
                          (implicit monoid: Monoid[A]): A =
        list.foldRight(monoid.empty)(monoid.combine)

      import cats.instances.int._ // for Monoid

      println {
        sumWithMonoid(List(1, 2, 3))
        // res13: Int = 6
      }

      println("-----------------------------------")
    }

    def chapter2 = {
      // Foldable in Cats

      // Cats' Foldable абстрагирует foldLeft и foldRight в класс типа.
      // Экземпляры Foldable определяют эти два метода и наследуют множество производных методов.
      // Cats предоставляет готовые экземпляры Foldable для нескольких типов данных Scala: List, Vector, LazyList, и Option.

      // Мы можем вызывать экземпляры как обычно, используя Foldable.apply
      // и вызывать их реализации foldLeft напрямую. Вот пример использования List:

      import cats.Foldable
      import cats.instances.list._ // for Foldable

      val ints = List(1, 2, 3)

      println {
        Foldable[List].foldLeft(ints, 0)(_ + _)
        // res0: Int = 6
      }

      // Другие последовательности, такие как Vector и LazyList работают таким же образом.
      // Вот пример использования Option, который рассматривается как последовательность из нуля или одного элемента:

      import cats.instances.option._ // for Foldable

      val maybeInt = Option(123)

      println {
        Foldable[Option].foldLeft(maybeInt, 10)(_ * _)
        // res1: Int = 1230
      }

      // Folding Right

      // Foldable определяет foldRight отличный от foldLeft, в терминах монады Eval:

      /*
            def foldRight[A, B] (fa: F[A], lb: Eval[B]) (f: (A, Eval[B]) => Eval[B]): Eval[B]
      */

      // Использование Eval означает, что сворачивание всегда безопасно для стека,
      // даже если определение коллекции по умолчанию foldRight не является таковым.
      // Например, реализация по умолчанию foldRight для LazyList не является безопасной для стека.
      // Чем длиннее ленивый список, тем больше требования к стеку для сворачивания.
      // Достаточно большой ленивый список вызовет StackOverflowError:

      import cats.Eval
      import cats.Foldable

      def bigData: LazyList[Int] = (1 to 1000000).to(LazyList)

      println {
        bigData.foldRight(0L)(_ + _)
        // возможен java.lang.StackOverflowError ...
      }

      // Использование Foldable заставляет нас использовать безопасные операции со стеком,
      // что устраняет исключение переполнения:

      import cats.instances.lazyList._ // for Foldable

      val eval: Eval[Long] =
        Foldable[LazyList].
          foldRight(bigData, Eval.now(0L)) { (num, eval) =>
            eval.map(_ + num)
          }

      println {
        eval.value
      }

      // Безопасность стека в стандартной библиотеке

      // Безопасность стека обычно не является проблемой при использовании стандартной библиотеки.
      // Наиболее часто используемые типы коллекций, такие как List и Vector, обеспечивают реализацию безопасного стека
      // в foldRight:

      (1 to 100000).toList.foldRight(0L)(_ + _)
      // res4: Long = 5000050000L
      (1 to 100000).toVector.foldRight(0L)(_ + _)
      // res5: Long = 5000050000L

      // Stream - это исключение из этого правила.
      // Какой бы тип данных мы ни использовали, полезно знать, что Eval нас поддерживает.

      println("-----------")

      // Folding с Monoids

      // Foldable предоставляет нам множество полезных методов, определенных поверх foldLeft.
      // Многие из них являются факсимиле знакомых методов из стандартной библиотеки:
      // find, exists, forall, toList, isEmpty, nonEmpty, и так далее:

      println(
          Foldable[Option].nonEmpty(Option(42)),
          // res6: Boolean = true

          Foldable[List].find(List(1, 2, 3))(_ % 2 == 0)
          // res7: Option[Int] = Some(2)
      )

      // В дополнение к этим знакомым методам, Cats предоставляет два метода, которые используют Monoids:

      // - combineAll(и его псевдоним fold) объединяет все элементы в последовательности, используя их Monoid;

      // - foldMap сопоставляет указанную пользователем функцию с последовательностью
      // и объединяет результаты с помощью Monoid.

      // Например, мы можем использовать combineAll для суммирования по a List[Int]

      import cats.instances.int._ // for Monoid

      println {
        Foldable[List].combineAll(List(1, 2, 3))
        // res8: Int = 6
      }

      // В качестве альтернативы мы можем использовать foldMap для преобразования каждого из них Int в String и объединения их:

      import cats.instances.string._ // for Monoid

      println {
        Foldable[List].foldMap(List(1, 2, 3))(_.toString)
        // res9: String = "123"
      }

      // Наконец, мы можем составить композицию Foldable,  для поддержки глубокого обхода вложенных последовательностей:

      import cats.instances.vector._ // for Monoid

      val int_s = List(Vector(1, 2, 3), Vector(4, 5, 6))

      println {
        (Foldable[List] compose Foldable[Vector]).combineAll(int_s)
        // res11: Int = 21
      }

      println("-----------")

      // Синтаксис для Foldable

      // Каждый метод в Foldable доступен в форме синтаксиса через cats.syntax.foldable.
      // В каждом случае первый аргумент метода в Foldable становится получателем вызова метода:

      import cats.syntax.foldable._ // for combineAll and foldMap

      println {
        List(1, 2, 3).combineAll
        // res12: Int = 6
      }

      println {
        List(1, 2, 3).foldMap(_.toString)
        // res13: String = "123"
      }

      // Явное против неявного

      // Помните, что Scala будет использовать экземпляр только Foldable в том случае,
      // если метод явно не доступен на приемнике.
      // Например, следующий код будет использовать версию, foldLeftопределенную на List:

      println {
        List(1, 2, 3).foldLeft(0)(_ + _)
      }
      // res14: Int = 6

      // тогда как следующий общий код будет использовать Foldable:

      def sum[F[_]: Foldable](values: F[Int]): Int =
        values.foldLeft(0)(_ + _)

      // Обычно нам не нужно беспокоиться об этом различии. Это особенность!
      // Мы вызываем нужный нам метод, а компилятор использует, Foldable когда это необходимо, чтобы гарантировать,
      // что наш код работает так, как ожидается.
      // Если нам нужна стекобезопасная реализация foldRight,
      // использования Eval в качестве аккумулятора достаточно,
      // чтобы заставить компилятор выбрать метод из Cats.

      println("-----------------------------------")
    }
  }

  object part2 {

    // Traverse

    // foldLeft и foldRight являются гибкими методами итерации,
    // но они требуют от нас выполнения большого объема работы по определению аккумуляторов и комбинаторных функций.
    // Тайп класс Traverse — это инструмент более высокого уровня,
    // который использует Applicative s для предоставления более удобного,
    // более законного шаблона для итерации.




    def chapter1 = {
      // Обход с помощью Futures

      // Мы можем продемонстрировать Traverse использование методов Future.traverse и Future.sequence
      // в стандартной библиотеке Scala.
      // Эти методы предоставляют Future специфические реализации шаблона traverse.
      // В качестве примера предположим, что у нас есть список имен хостов сервера
      // и метод опроса хоста на предмет его работоспособности:

      import scala.concurrent._
      import scala.concurrent.duration._
      import scala.concurrent.ExecutionContext.Implicits.global

      val hostnames = List(
        "alpha.example.com",
        "beta.example.com",
        "gamma.demo.com"
      )

      def getUptime(hostname: String): Future[Int] =
        Future(hostname.length * 60) // just for demonstration

      // Теперь предположим, что мы хотим опросить все хосты и собрать все их аптаймы.
      // Мы не можем просто сделать map на hostnames, потому что результат — a List[Future[Int]]
      // — будет содержать более одного Future. Нам нужно свести результаты к одному, Future, чтобы получить что-то,
      // на чем мы можем блокироваться.
      // Давайте начнем с того, что сделаем это вручную с помощью фолда:

      val allUptimes1: Future[List[Int]] =
        hostnames.foldLeft(Future(List.empty[Int])) {
          (accum, host) =>
            val uptime = getUptime(host)
            for {
              accum  <- accum
              uptime <- uptime
            } yield accum :+ uptime
        }

      println {
        Await.result(allUptimes1, 1.second)
        // res0: List[Int] = List(1020, 960, 840)
      }

      // Интуитивно, мы итерируем по hostnames, вызываем func для каждого элемента и объединяем результаты в список.
      // Это звучит просто, но код довольно громоздкий из-за необходимости создания и объединения Futures на каждой итерации.
      // Мы можем значительно улучшить вещи, используя Future.traverse, который специально создан для этого шаблона:

      val allUptimes2: Future[List[Int]] =
        Future.traverse(hostnames)(getUptime)

      println {
        Await.result(allUptimes2, 1.second)
        // res2: List[Int] = List(1020, 960, 840)
      }

      // Это гораздо понятнее и лаконичнее — давайте посмотрим, как это работает.
      // Если игнорировать отвлекающие факторы вроде CanBuildFrom и ExecutionContext, то реализация Future.traverse
      // в стандартной библиотеке выглядит так:

      def traverse[A, B](values: List[A])
                        (func: A => Future[B]): Future[List[B]] =
        values.foldLeft(Future(List.empty[B])) { (accum, host) =>
          val item = func(host)
          for {
            accum <- accum
            item  <- item
          } yield accum :+ item
        }

      // Это по сути то же самое, что и наш пример кода выше. Future.traverse абстрагируется
      // от боли сворачивания и определения аккумуляторов и функций комбинирования.
      // Это дает нам чистый интерфейс высокого уровня, чтобы делать то, что мы хотим:

      // - начать с List[A];
      // - обеспечить функцию A => Future[B];
      // - в конечном итоге получается Future[List[B]].

      // Стандартная библиотека также предоставляет другой метод, Future.sequence
      // который предполагает, что мы начинаем с List[Future[B]] и нам не нужно предоставлять функцию идентичности:

      /*
        object Future {
           def sequence[B](futures: List[Future[B]]): Future[List[B]] =
                traverse(futures)(identity)

              // etc...
        }
      */

      // В этом случае интуитивное понимание еще проще:

      // - начать с List[Future[A]];
      // - в конечном итоге получается Future[List[A]].

      // Future.traverse и Future.sequence решают очень конкретную задачу:
      // они позволяют нам перебирать последовательность Future и накапливать результат.
      // Упрощенные примеры выше работают только с List, но реальные Future.traverse и Future.sequence
      // работают с любой стандартной коллекцией Scala.

      //Класс типов Cats Traverse обобщает эти шаблоны для работы с любым типом Applicative:
      // Future, Option, Validated, и так далее.

      // В следующих разделах мы подойдем к этому в два этапа: сначала мы обобщим по Applicative,
      // затем мы обобщим по типу последовательности.
      // Мы получим чрезвычайно ценный инструмент, который упрощает многие операции с последовательностями
      // и другими типами данных.

      println("-------------------------------")

    }

    def chapter2 = {

      val hostnames = List(
        "alpha.example.com",
        "beta.example.com",
        "gamma.demo.com"
      )

      def getUptime(hostname: String): Future[Int] =
        Future(hostname.length * 60) // just for demonstration


      // Обход с помощью Applicatives

      // Если присмотреться, то можно увидеть, что мы можем переписать traverse в терминах Applicative.
      // Наш аккумулятор из примера выше:

      // Future(List.empty[Int])
      // эквивалентно Applicative.pure:

      import cats.Applicative
      import cats.instances.future._   // for Applicative
      import cats.syntax.applicative._ // for pure

      List.empty[Int].pure[Future]

      // Наш комбинатор, который раньше был таким:

      def oldCombine(
                      accum : Future[List[Int]],
                      host  : String
                    ): Future[List[Int]] = {
        val uptime = getUptime(host)
        for {
          accum  <- accum
          uptime <- uptime
        } yield accum :+ uptime
      }

      // теперь эквивалентно Semigroupal.combine:

      import cats.syntax.apply._ // for mapN

      // Combining accumulator and hostname using an Applicative:
      def newCombine(accum: Future[List[Int]], host: String): Future[List[Int]] =
            (accum, getUptime(host)).mapN(_ :+ _)

      // Подставив эти фрагменты обратно в определение, traverse мы можем обобщить его для работы с любым Applicative:


      def listTraverse[F[_]: Applicative, A, B] (list: List[A]) (func: A => F[B]): F[List[B]] =
        list.foldLeft(List.empty[B].pure[F]) { (accum, item) => (accum, func(item)).mapN(_ :+ _) }

      def listSequence[F[_]: Applicative, B] (list: List[F[B]]): F[List[B]] =
        listTraverse(list)(identity)

      // Для повторной реализации нашего примера безотказной работы мы можем использовать listTraverse:

      val totalUptime = listTraverse(hostnames)(getUptime)

      println {
        Await.result(totalUptime, 1.second)
        // res5: List[Int] = List(1020, 960, 840)
      }

      // или мы можем использовать его с другими Applicative типами данных, как показано в следующих упражнениях.

      println("---------")

      // Упражнение: обход векторов

      // Каков результат следующего?

      val sequence1: Vector[List[Int]] = listSequence(List(Vector(1, 2), Vector(3, 4)))

      println {
        sequence1
      }

      // А как насчет списка из трех параметров?

      val sequence2: Vector[List[Int]] =  listSequence(List(Vector(1, 2), Vector(3, 4), Vector(5, 6)))

      println {
        sequence2
      }

      println("---------")

      // Упражнение: Обход с вариантами

      // Вот пример использования Options:

      import cats.instances.option._ // for Applicative

      def process1(inputs: List[Int]): Option[List[Int]] =
        listTraverse(inputs)(n => if(n % 2 == 0) Some(n) else None)

      // Какой тип возвращаемого значения у этого метода? Что он выдает для следующих входных данных?

      println {
        process1(List(2, 4, 6))
        // res12: Option[List[Int]] = Some(List(2, 4, 6))
      }

      println {
        process1(List(1, 2, 3))
        // res13: Option[List[Int]] = None
      }

      // Аргументы listTraverse имеют типы List[Int] и Int => Option[Int],
      // поэтому возвращаемый тип — Option[List[Int]].
      // Опять же, Option является монадой, поэтому полугрупповая combine функция следует из flatMap.
      // Таким образом, семантика — это обработка ошибок с быстрым отказом:
      // если все входы четные, мы получаем список выходов. В противном случае мы получаем None:

      println("---------")

      // Упражнение: Обход с Validated

      // Наконец, вот пример использования Validated:

      import cats.data.Validated
      import cats.instances.list._ // for Monoid

      type ErrorsOr[A] = Validated[List[String], A]

      def process2(inputs: List[Int]): ErrorsOr[List[Int]] =
        listTraverse(inputs) { n =>
          if(n % 2 == 0) {
            Validated.valid(n)
          } else {
            Validated.invalid(List(s"$n is not even"))
          }
        }


      // Что дает этот метод для следующих входных данных?

      println {
        process2(List(2, 4, 6))
        // res17: ErrorsOr[List[Int]] = Valid(List(2, 4, 6))
      }

      println {
        process2(List(1, 2, 3))
        // res18: ErrorsOr[List[Int]] = Invalid(List("1 is not even", "3 is not even"))
      }

      println("-------------------------------")
    }

    def chapter3 = {

      val hostnames = List(
        "alpha.example.com",
        "beta.example.com",
        "gamma.demo.com"
      )

      def getUptime(hostname: String): Future[Int] =
        Future(hostname.length * 60) // just for demonstration

      // Traverse в Cats

      // Наши методы listTraverse и listSequence работают с любым типом Applicative,
      // но они работают только с одним типом последовательности: List.
      // Мы можем обобщать различные типы последовательностей, используя Тайп-класс, что приводит нас к Cats Traverse.
      // Вот сокращенное определение:

      /*

              package cats

              trait Traverse[F[_]] {
                def traverse[G[_]: Applicative, A, B]
                (inputs: F[A])(func: A => G[B]): G[F[B]]

                def sequence[G[_]: Applicative, B]
                (inputs: F[G[B]]): G[F[B]] =
                  traverse(inputs)(identity)
              }

      */

      // Cats предоставляет экземпляры Traverse для List, Vector, Stream, Option, Either и множество других типов.
      // Мы можем вызывать экземпляры как обычно, используя Traverse.apply и использовать методы traverse и sequence,
      // как описано в предыдущем разделе:

      import cats.Traverse

      import cats.instances.future._ // for Applicative
      import cats.instances.list._   // for Traverse

      val totalUptime: Future[List[Int]] =
        Traverse[List].traverse(hostnames)(getUptime)

      println {
        Await.result(totalUptime, 1.second)
        // res0: List[Int] = List(1020, 960, 840)
      }


      val numbers = List(Future(1), Future(2), Future(3))

      val numbers2: Future[List[Int]] =
        Traverse[List].sequence(numbers)

      println {
        Await.result(numbers2, 1.second)
        // res1: List[Int] = List(1, 2, 3)
      }

      // Существуют также синтаксические версии методов, импортированные через cats.syntax.traverse:

      import cats.syntax.traverse._ // for sequence and traverse

      println {

        Await.result(hostnames.traverse(getUptime), 1.second)
        // res2: List[Int] = List(1020, 960, 840)
      }


      println {
        Await.result(numbers.sequence[Future,Int], 1.second)
        //Await.result(numbers.traverse(identity), 1.second)
        // res3: List[Int] = List(1, 2, 3)
      }

      // Как видите, это гораздо более компактно и читабельно, чем foldLeft код,
      // с которого мы начали ранее в этой главе!

      println("-------------------------------")
    }
  }
  part1.chapter1
  part1.chapter2

  part2.chapter1
  part2.chapter2
  part2.chapter3

}

// Резюме

// В этой главе мы познакомились с Foldable и Traverse, двумя классами типов для итерации по последовательностям.

// Foldable абстрагирует методы foldLeft и foldRight, которые мы знаем из коллекций в стандартной библиотеке.
// Он добавляет безопасные для стека реализации этих методов к нескольким дополнительным типам данных
// и определяет множество полезных в зависимости от ситуации дополнений.
// Тем не менее, Foldable не вводит много того, чего мы уже не знали.

// Реальная сила исходит от Traverse,
// который абстрагирует и обобщает методы traverse и sequence,
// которые мы знаем из Future.
// Используя эти методы, мы можем превратить F[G[A]] в G[F[A]]
// для любого F с экземпляром Traverse и любой G с экземпляром Applicative.
// С точки зрения сокращения, которое мы получаем в строках кода,
// Traverse является одним из самых мощных шаблонов в этой книге.
// Мы можем сократить folds из многих строк до одной foo.traverse.


