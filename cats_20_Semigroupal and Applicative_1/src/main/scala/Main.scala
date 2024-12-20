

import cats.Semigroupal
import cats.syntax.apply._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.language.postfixOps


object Main extends App {

  println("Изучаем Cats!".toUpperCase)
  println("Semigroupal and Applicative")

  object part1 {
    def chapter1 = {
      // Semigroupal and Applicative (Полугрупповой и аппликативный)

      // В предыдущих главах мы увидели, как функторы и монады позволяют нам упорядочивать операции с помощью map и flatMap.
      // Хотя функторы и монады являются чрезвычайно полезными абстракциями,
      // существуют определенные типы потока программы, которые они не могут представить.

      // Одним из таких примеров является проверка формы.
      // Когда мы проверяем форму, мы хотим вернуть все ошибки пользователю,
      // а не останавливаться на первой же ошибке, с которой сталкиваемся.
      // Если мы моделируем это с помощью монады вроде Either, мы быстро терпим неудачу и теряем ошибки.
      // Например, код ниже терпит неудачу при первом вызове parseInt и не идет дальше:

      import cats.syntax.either._ // for catchOnly

      def parseInt(str: String): Either[String, Int] =
        Either.catchOnly[NumberFormatException](str.toInt).
          leftMap(_ => s"Couldn't read $str")

      val resForm: Either[String, Int] = for {
        a <- parseInt("a")
        b <- parseInt("b")
        c <- parseInt("c")
      } yield (a + b + c)
      // res0: Either[String, Int] = Left("Couldn't read a")

      println(resForm)

      // Другим примером является параллельная оценка Future.
      // Если у нас есть несколько длительно выполняющихся независимых задач,
      // имеет смысл выполнять их одновременно. Однако монадическое понимание позволяет нам запускать их только последовательно.
      // map и flatMap не вполне способны уловить то, что мы хотим, поскольку они предполагают, что каждое вычисление зависит от предыдущего:


      /*
          // context2 is dependent on value1:
          context1.flatMap(value1 => context2)
      */

      // Вызовы parseInt и Future.apply выше независимы друг от друга, но map и flatMap не могут это использовать.
      // Нам нужна более слабая конструкция — та, которая не гарантирует последовательность — чтобы достичь желаемого результата.
      // В этой главе мы рассмотрим три класса типов, которые поддерживают этот шаблон:

      // - Semigroupal охватывает понятие составления пар контекстов.
      // Cats предоставляет cats.syntax.apply модуль, который используя Semigroupal и Functor,
      // позволяет пользователям упорядочивать функции с несколькими аргументами.

      // - Parallel преобразует типы с Monad экземпляром в связанный тип с Semigroupal экземпляром.

      // - Applicative расширяет Semigroupal и Functor. Он обеспечивает способ применения функций к параметрам в контексте.
      // Applicative является источником pure метода, который мы представили в Главе 4. (Монады)

      // Applicatives часто формулируются в терминах применения функций, а не полугрупповой формулировки, которая подчеркивается в Cats.
      // Эта альтернативная формулировка обеспечивает связь с другими библиотеками и языками, такими как Scalaz и Haskell.
      // Мы рассмотрим различные формулировки Applicative, а также отношения между Semigroupal, Functor, Applicative, и Monad, ближе к концу главы.

      println("------------------")

    }

    def chapter2 = {
      // Semigroupal

      // cats.Semigroupal— это класс типов, который позволяет нам объединять контексты.
      // Если у нас есть два объекта типа F[A]и F[B], a Semigroupal[F]позволяет нам объединить их для формирования F[(A, B)].
      // Его определение в Cats:

      /*
          trait Semigroupal[F[_]] {
            def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
          }
      */

      // Как мы обсуждали в начале этой главы, параметры fa и fb независимы друг от друга:
      // мы можем вычислить их в любом порядке перед передачей в product.
      // Это контрастирует с flatMap, который накладывает строгий порядок на свои параметры.
      // Это дает нам больше свободы при определении экземпляров, Semigroupal чем при определении Monads.

      // Объединение двух контекстов

      // Пока Semigroup позволяет нам объединять значения,
      // Semigroupal позволяет нам объединять контексты.
      // Давайте объединим некоторые Options в качестве примера:
      val semi1 = Semigroupal[Option].product(Some(123), Some("abc"))
      println(semi1)

      // Если оба параметра являются экземплярами Some, мы получаем кортеж значений внутри.
      // Если любой из параметров оценивается как None, весь результат будет None:

      val semi2 = Semigroupal[Option].product(None, Some("abc"))
      // res2: Option[Tuple2[Nothing, String]] = None

      val semi3 = Semigroupal[Option].product(Some(123), None)
      // res3: Option[Tuple2[Int, Nothing]] = None

      println(semi2)
      println(semi3)

      println("------------------")
    }

    def chapter3 = {
      // Объединение трех и более контекстов
      // Сопутствующий объект для Semigroupal определяет набор методов поверх product.
      // Например, методы tuple2 ... tuple22 обобщают product до различных арностей:

      import cats.instances.option._ // for Semigroupal

      val semi1 = Semigroupal.tuple3(Option(1), Option(2), Option(3))
      // res4: Option[(Int, Int, Int)] = Some((1, 2, 3))
      val semi2 = Semigroupal.tuple3(Option(1), Option(2), Option.empty[Int])
      // res5: Option[(Int, Int, Int)] = None
      println(semi1)
      println(semi2)

      // Методы map2 ... map22 применяют указанную пользователем функцию к значениям внутри 2–22 контекстов:

      val semi3 = Semigroupal.map3(Option(1), Option(2), Option(3))(_ + _ + _)
      // res6: Option[Int] = Some(6)

      val semi4 = Semigroupal.map2(Option(1), Option.empty[Int])(_ + _)
      // res7: Option[Int] = None

      println {
        semi3
      }
      println {
        semi4
      }

      // Существуют также методы contramap2 ...  contramap22 и imap2 ... imap22, которым требуются экземпляры Contravariant и Invariant соответственно.

      // Законы Semigrupal
      // Cуществует только один закон Semigroupal: метод product должен быть ассоциативным.

      /*
           product(a, product(b, c)) == product(product(a, b), c)
      */

      println("------------------")
    }

    def chapter4 = {
      // Apply Syntax

      // Cats предоставляет удобный синтаксис применения, который обеспечивает сокращение для методов, описанных выше.
      // Мы импортируем синтаксис из cats.syntax.apply.
      // Вот пример:

      import cats.instances.option._ // for Semigroupal
      import cats.syntax.apply._     // for tupled and mapN


      // Мы можем использовать тот же трюк для кортежей, содержащих до 22 значений.
      // Cats определяет отдельный tupled метод для каждой арности:

      val tupled =(Option(123), Option("abc"), Option(true)).tupled
      // res9: Option[(Int, String, Boolean)] = Some((123, "abc", true))
      println(tupled)

      // В дополнение к tupled, синтаксис применения Cats предоставляет метод mapN,
      // который принимает неявное значение Functor и функцию правильной арности для объединения значений.

      final case class Cat(name: String, born: Int, color: String)

      val optCat = (
        Option("Garfield"),
        Option(1978),
        Option("Orange & black")
      ).mapN(Cat.apply)
      // res10: Option[Cat] = Some(Cat("Garfield", 1978, "Orange & black"))
      println(optCat)

      // Из всех упомянутых здесь методов наиболее распространенным является использование mapN.

      // Внутренне mapN использует Semigroupal для извлечения значений из Option и Functor для применения значений к функции.

      // Приятно видеть, что этот синтаксис проверяется на соответствие типам.
      // Если мы предоставим функцию, которая принимает неправильное количество или типы параметров, мы получим ошибку компиляции:

        /*
              val add: (Int, Int) => Int = (a, b) => a + b
              // add: (Int, Int) => Int = <function2>

              (Option(1), Option(2), Option(3)).mapN(add)
              // error: ':' expected but '(' found.
              //   Option("Garfield"),
              //         ^
              // error: identifier expected but '}' found.

              (Option("cats"), Option(true)).mapN(add)
              // error: ':' expected but '(' found.
              //   Option("Garfield"),
              //         ^
              // error: identifier expected but '}' found.
        */

      println("------------------")
    }

    def chapter5 = {
      // Необычные функторы и синтаксис Apply

      // Синтаксис Apply также имеет contramapN и imapN методы, которые принимают контравариантные и инвариантные функторы.
      // Например, мы можем объединить Monoid s с помощью Invariant.
      // Вот пример:

      import cats.Monoid
      import cats.instances.int._        // for Monoid
      import cats.instances.invariant._  // for Semigroupal
      import cats.instances.list._       // for Monoid
      import cats.instances.string._     // for Monoid
      import cats.syntax.apply._         // for imapN

      final case class Cat(
                            name: String,
                            yearOfBirth: Int,
                            favoriteFoods: List[String]
                          )

      val tupleToCat: (String, Int, List[String]) => Cat = Cat.apply _

      val catToTuple: Cat => (String, Int, List[String]) = cat => (cat.name, cat.yearOfBirth, cat.favoriteFoods)

      implicit val catMonoid: Monoid[Cat] = (
        Monoid[String],
        Monoid[Int],
        Monoid[List[String]]
      ).imapN(tupleToCat)(catToTuple)

      // Наш Monoid позволяет нам создавать «пустые» Cats и складывать Cats , используя синтаксис из Главы 2:

      import cats.syntax.semigroup._ // for |+|

      val garfield   = Cat("Garfield", 1978, List("Lasagne"))
      val heathcliff = Cat("Heathcliff", 1988, List("Junk Food"))

      val sumCat: Cat = garfield |+| heathcliff
      // res14: Cat = Cat("GarfieldHeathcliff", 3966, List("Lasagne", "Junk Food"))
      println(sumCat)

      println("------------------")
    }

    def chapter6 = {

      // Semigroupal Applied к разным типам

      // Semigroupal не всегда обеспечивает ожидаемое нами поведение, особенно для типов, которые являются инстансами Monad.
      // Мы видели поведение Semigroupal для Option.
      // Давайте рассмотрим некоторые примеры для других типов.

      // Future

      // Семантика Future обеспечивает параллельное, а не последовательное выполнение:

      import cats.Semigroupal
      import cats.instances.future._ // for Semigroupal
      import scala.concurrent._
      import scala.concurrent.duration._
      import scala.concurrent.ExecutionContext.Implicits.global

      val futurePair = Semigroupal[Future].product(Future("Hello"), Future(123))

      val resFuture1: (String, Int) = Await.result(futurePair, 1 second)
      // res0: (String, Int) = ("Hello", 123)

      println(resFuture1)

      // Оба Future  начинают выполняться в тот момент, когда мы их создаем,
      // поэтому они уже вычисляют результаты к тому времени, как мы вызываем product.
      // Мы можем использовать синтаксис apply для zip фиксированного количества Futures:

      import cats.syntax.apply._ // for mapN


      case class Cat(
                      name: String,
                      yearOfBirth: Int,
                      favoriteFoods: List[String]
                    )

      val futureCat: Future[Cat] = (
        Future("Garfield"),
        Future(1978),
        Future(List("Lasagne"))
      ).mapN(Cat.apply)


      val resFuture2 = Await.result(futureCat, 1 second)
      // res1: Cat = Cat("Garfield", 1978, List("Lasagne"))
      println{
        resFuture2
      }

      // Список
      // Объединение Lists с Semigroupal дает некоторые потенциально неожиданные результаты.
      // Мы могли бы ожидать, что код вроде следующего сожмет списки,
      // но на самом деле мы получаем декартово произведение их элементов:


      import cats.instances.list._ // for Semigroupal

      val semList: Seq[(Int, Int)] = Semigroupal[List].product(List(1, 2), List(3, 4))
      // res2: List[(Int, Int)] = List((1, 3), (1, 4), (2, 3), (2, 4))
      println {
        semList
      }

      // Это, возможно, удивительно.
      // Сжатие списков, как правило, более распространенная операция.
      // Мы увидим, почему мы получаем такое поведение, через минуту.

      // Either

      // Мы начали эту главу с обсуждения fail-fast против accumulating error-handling.
      // Мы могли бы ожидать, что product applied to Either будет накапливать ошибки вместо fail fast.
      // Опять же, возможно, это удивительно, мы обнаруживаем, что product реализует то же поведение fail-fast, что и flatMap:

      import cats.instances.either._ // for Semigroupal

      type ErrorOr[A] = Either[Vector[String], A]

      val error1 = Semigroupal[ErrorOr].product(
        Left(Vector("Error 1")),
        Left(Vector("Error 2"))
      )
      // res3: ErrorOr[Tuple2[Nothing, Nothing]] = Left(Vector("Error 1"))

      println {
        error1
      }

      // В этом примере product мы видим первую ошибку и останавливаемся,
      // хотя можно проверить второй параметр и увидеть, что это тоже ошибка.


      println("------------------")
    }

    def chapter7 = {
      // Semigroupal примененый к монадам

      // Причина удивительных результатов для List и Either в том, что они обе - монады.
      // Если у нас есть монада, мы можем реализовать product следующим образом.

      import cats.Monad
      import cats.syntax.functor._ // for map
      import cats.syntax.flatMap._ // for flatmap

      def product[F[_]: Monad, A, B](fa: F[A], fb: F[B]): F[(A,B)] =
        fa.flatMap(a =>
          fb.map(b =>
            (a, b)
          )
        )

      // Было бы очень странно, если бы у нас была разная семантика для product в зависимости от того, как мы ее реализовали.
      // Чтобы обеспечить согласованную семантику, Cats' Monad (который расширяет Semigroupal) предоставляет стандартное определение product
      // в терминах map и , flatMap как мы показали выше.

      // Даже наши результаты для Future являются игрой света.
      // flatMap обеспечивает последовательное упорядочивание, поэтому product обеспечивает то же самое.
      // Параллельное выполнение, которое мы наблюдаем, происходит, потому что наш компонент Future начинает работать до того, как мы вызываем product.
      // Это эквивалентно классическому шаблону create-then-flatMap:

      val a = Future("Future 1")
      val b = Future("Future 2")

      for {
        x <- a
        y <- b
      } yield (x, y)

      // Так зачем вообще беспокоиться Semigroupal?
      // Ответ в том, что мы можем создавать полезные типы данных, которые имеют экземпляры Semigroupal(и Applicative), но не Monad.
      // Это освобождает нас для реализации product разными способами.
      // Мы рассмотрим это подробнее через мгновение, когда рассмотрим альтернативный тип данных для обработки ошибок.

      // Упражнение: Произведение списков

      // Почему product for List производит декартово произведение? Мы видели пример выше. Вот он снова.

      Semigroupal[List].product(List(1, 2), List(3, 4))
      // res5: List[(Int, Int)] = List((1, 3), (1, 4), (2, 3), (2, 4))

      // Мы также можем записать это в терминах tupled.

      (List(1, 2), List(3, 4)).tupled
      // res6: List[(Int, Int)] = List((1, 3), (1, 4), (2, 3), (2, 4))

      // Это упражнение проверяет, что вы поняли определение product в терминах flatMap и map.

      import cats.syntax.functor._ // for map
      import cats.syntax.flatMap._ // for flatMap

      /*
          def product[F[_]: Monad, A, B](x: F[A], y: F[B]): F[(A, B)] = x.flatMap(a => y.map(b => (a, b)))
      */

      // Этот код эквивалентен коду для понимания:

      def product_flat_map[F[_]: Monad, A, B](x: F[A], y: F[B]): F[(A, B)] =
        for {
          a <- x
          b <- y
        } yield (a, b)

      // Семантика — flatMap это то, что обуславливает поведение для List и Either:

      import cats.instances.list._ // for Semigroupal

      println {
        product_flat_map(List(1, 2), List(3, 4))
      }
      // res9: List[(Int, Int)] = List((1, 3), (1, 4), (2, 3), (2, 4))

      println("------------------")
    }

    def chapter8 = {

      // Parallel

      // В предыдущем разделе мы увидели, что при вызове product на типах, являющихся экземплярами Monad,
      // мы получаем последовательную семантику. Это имеет смысл с точки зрения сохранения согласованности с реализациями product в терминах flatMap и map.
      // Однако это не всегда то, что нам нужно.
      // Класс типа Parallel и связанный с ним синтаксис позволяют нам получать доступ к альтернативной семантике для определенных монад.

      // Мы видели, как product метод Either останавливается при первой ошибке.

      import cats.Semigroupal
      import cats.instances.either._ // for Semigroupal

      type ErrorOr[A] = Either[Vector[String], A]
      val error1: ErrorOr[Int] = Left(Vector("Error 1"))
      val error2: ErrorOr[Int] = Left(Vector("Error 2"))
      val error3: ErrorOr[Int] = Left(Vector("Error 3"))

      println {
        Semigroupal[ErrorOr].product(error1, error2)
        // res0: ErrorOr[(Int, Int)] = Left(Vector("Error 1"))
      }

      // Мы также можем записать это, используя tupled сокращенную запись.


      import cats.instances.vector._ // for Semigroup on Vector

      println {
        (error1 , error2).tupled
        // res1: ErrorOr[(Int, Int)] = Left(Vector("Error 1"))
      }

      // Чтобы собрать все ошибки, мы просто заменяем tupled его «параллельной» версией, называемой parTupled.

      import cats.syntax.parallel._ // for parTupled
      println {
        (error1, error2).parTupled
        // res2: ErrorOr[(Int, Int)] = Left(Vector("Error 1", "Error 2"))
      }

      // Обратите внимание, что возвращаются обе ошибки!
      // Это поведение не является особым для использования Vector в качестве типа ошибки.
      // Любой тип, имеющий экземпляр, Semigroup будет работать. Например, здесь мы используем List вместо этого.

      import cats.instances.list._ // for Semigroup on List

      type ErrorOrList[A] = Either[List[String], A]
      val errStr1: ErrorOrList[Int] = Left(List("error 1"))
      val errStr2: ErrorOrList[Int] = Left(List("error 2"))

      println {
        (errStr1, errStr2).parTupled
        // res3: ErrorOrList[(Int, Int)] = Left(List("error 1", "error 2"))
      }

      // Существует множество методов синтаксиса, предоставляемых Parallel для методов на Semigroupal и связанных типов,
      // но наиболее часто используемый — parMapN. Вот пример parMapN в ситуации обработки ошибок.


      val success1: ErrorOr[Int] = Right(1)
      val success2: ErrorOr[Int] = Right(2)
      val addTwo = (x: Int, y: Int) => x + y

      println {
        (error1, error2).parMapN(addTwo)
        // res4: ErrorOr[Int] = Left(Vector("Error 1", "Error 2"))
      }

      println {
        (success1, success2).parMapN(addTwo)
        // res5: ErrorOr[Int] = Right(3)
      }

      println("------------------")
    }

    def chapter9 = {

      // Давайте разберемся, как это Parallel работает. Определение ниже — это ядро Parallel.

      /*

                trait Parallel[M[_]] {
                type F[_]

                def applicative: Applicative[F]
                def monad: Monad[M]
                def parallel: ~>[M, F]
              }

      */

      // Это говорит нам, что если существует Parallel экземпляр для некоторого конструктора типа M, то:

      // - должен быть Monad экземпляр для M;
      // - существует связанный конструктор типа F, имеющий Applicative экземпляр;
      // - мы можем конвертировать M в F.

      // Мы не видели этого ~> раньше.
      // Это псевдоним типа для FunctionK и выполняет преобразование из M в F.
      // Обычная функция A => Bпреобразует значения типа A в значения типа B.
      // Помните, что M и F не являются типами - они являются конструкторами типов.
      // FunctionK M ~> F — это функция из значения с типом M[A] в значение с типом F[A].
      // Давайте рассмотрим быстрый пример, определив, FunctionK который преобразует an Option в a List.

      import cats.arrow.FunctionK

      object optionToList extends FunctionK[Option, List] {
        def apply[A](fa: Option[A]): List[A] =
          fa match {
            case None    => List.empty[A]
            case Some(a) => List(a)
          }
      }

      println {
        optionToList(Some(1))
        // res6: List[Int] = List(1)
      }

      println {
        optionToList(None)
        // res7: List[Nothing] = List()
      }

      // Поскольку параметр типа A является общим, FunctionK не может проверять какие-либо значения, содержащиеся в конструкторе типа M.
      // Преобразование должно выполняться исключительно в терминах структуры конструкторов типа M и F.
      // Мы можем в optionToList вышеприведенном примере это действительно так.

      // Итак, вкратце, Parallel позволяет нам взять тип, имеющий экземпляр монады,
      // и преобразовать его в некоторый связанный тип, имеющий вместо этого аппликативный (или полугрупповой) экземпляр.
      // Этот связанный тип будет иметь некоторую полезную альтернативную семантику.
      // Мы видели выше случай, когда связанный аппликативный for Either допускает накопление ошибок вместо семантики fail-fast.

      // Теперь мы увидели, Parallel - пришло время наконец узнать больше о Applicative.


      // Упражнение: Параллельный список.

      // List
      // Есть ли Parallel экземпляр? Если да, то что делает Parallel экземпляр?

      // List имеет Parallel экземпляр, и он устраняет необходимость создания декартова произведения List.
      // Мы можем это увидеть, написав небольшой фрагмент кода.

      import cats.instances.list._ // for Semigroup on List
      import cats.syntax.parallel._ // for parTupled


      println {
        (List(1, 2), List(3, 4)).tupled
        // res8: List[(Int, Int)] = List((1, 3), (1, 4), (2, 3), (2, 4))
      }

      println {
        (List(1, 2), List(3, 4)).parTupled
        // res9: List[(Int, Int)] = List((1, 3), (2, 4))
      }

      println("------------------")
    }

    def chapter10 = {
      // Apply and Applicative

      // Semigroupals нечасто упоминаются в более широкой литературе по функциональному программированию.
      // Они предоставляют подмножество функциональности связанного класса типов,
      // называемого АППЛИКАТИВНЫЙ ФУНКТОР (сокращенно «applicative»).

      // Semigroupal и Applicative эффективно предоставляют альтернативные кодировки того же понятия объединения контекстов.
      // Обе кодировки введены в одной и той же статье 2008 года Конором Макбрайдом и Россом Патерсоном.

      // http://www.staff.city.ac.uk/~ross/papers/Applicative.html

      // Cats моделирует аппликативные объекты с использованием двух классов типов.
      // Первый, cats.Apply, расширяет Semigroupal и Functor и добавляет метод ap,
      // который применяет параметр к функции в контексте.
      // Второй, cats.Applicative, расширяет Apply и добавляет pure метод, представленный в Главе 4.
      // Вот упрощенное определение в коде:

      /*

                trait Apply[F[_]] extends Semigroupal[F] with Functor[F] {
                  def ap[A, B](ff: F[A => B])(fa: F[A]): F[B]

                  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
                    ap(map(fa)(a => (b: B) => (a, b)))(fb)
                }

                trait Applicative[F[_]] extends Apply[F] {
                  def pure[A](a: A): F[A]
                }

      */

      // Разбивая это на части, метод ap применяет  ff: F[A => B] к параметру fa: F[A] в контексте F[_].
      // Метод product из Semigroupal определяется в терминах ap и map.

      // Не беспокойтесь слишком сильно о реализации product — ее трудно читать,
      // а детали не особенно важны. Главное, что существует тесная связь между product, ap, и map
      // и это позволяет определить любой из них в терминах двух других.

      // Applicative также вводит pure метод.
      // Это то же самое pure, что мы видели в Monad.
      // Он создает новый аппликативный экземпляр из развернутого значения.
      // В этом смысле Applicative относится к Apply, как Monoid относится к Semigroup.

      // Sequencing Type Classes
    }

    def chapter11 = {
      // Иерархия Sequencing Type Classes

      // С введением Apply и Applicative мы можем уменьшить масштаб и увидеть целое семейство классов типов,
      // которые занимаются вычислениями последовательности разными способами.
      // На рисунке ниже показана связь между классами типов, рассматриваемыми в этой книге.
      // Полную картину смотрите в инфографике Роба Норриса.
      // https://github.com/tpolecat/cats-infographic

      // resources/MonadTypeClassHierarchy.png

      // Каждый класс типов в иерархии представляет собой определенный набор семантики последовательности,
      // вводит набор характерных методов и определяет функциональность своих супертипов в их терминах:

      // - каждая монада является аппликативной;
      // - каждый аппликативный является полугрупповым;
      // - и так далее.

      // Ввиду закономерной природы отношений между классами типов,
      // отношения наследования постоянны во всех экземплярах класса типов.
      // Apply определяет product в терминах ap и map;
      // Monad определяет product, ap, и map, в терминах pure и flatMap.

      // Чтобы проиллюстрировать это, давайте рассмотрим два гипотетических типа данных:

      // Foo является монадой. Имеет экземпляр Monad класса типа, который реализует pure и flatMap
      // и наследует стандартные определения product, map, и ap;

      // Bar является аппликативным функтором. Он имеет экземпляр,
      // Applicative который реализует pure и ap и наследует стандартные определения product и map.

      // Что мы можем сказать об этих двух типах данных, не зная больше об их реализации?

      // Мы знаем строго больше о Foo чем Bar:
      // Monad является подтипом Applicative, поэтому мы можем гарантировать свойства Foo(а именно flatMap),
      // которые мы не можем гарантировать с Bar.
      // И наоборот, мы знаем, что Bar может иметь более широкий диапазон поведения, чем Foo.
      // У него меньше законов, которым нужно подчиняться (нет flatMap),
      // поэтому он может реализовывать поведение, которое Foo не может.

      // Это демонстрирует классический компромисс между мощностью (в математическом смысле) и ограничением.
      // Чем больше ограничений мы накладываем на тип данных, тем больше у нас гарантий относительно его поведения,
      // но тем меньше поведений мы можем смоделировать.

      // Монады оказались сладкой ниточкой в этом компромиссе.
      // Они достаточно гибки, чтобы моделировать широкий спектр поведений, и достаточно ограничительны,
      // чтобы давать надежные гарантии относительно этих поведений.

      // Однако есть ситуации, когда монады не являются подходящим инструментом для работы!
      // Иногда мы хотим тайской еды, а буррито просто не удовлетворяют.

      // В то время как монады налагают строгую последовательность на моделируемые ими вычисления,
      // аппликативные и полугрупповые не налагают таких ограничений.
      // Это ставит их в другую золотую середину иерархии.
      // Мы можем использовать их для представления классов параллельных/независимых вычислений, которые монады не могут.

      // Мы выбираем нашу семантику, выбирая наши структуры данных.
      // Если мы выбираем монаду, мы получаем строгую последовательность.
      // Если мы выбираем аппликатив, мы теряем способность flatMap.
      // Это компромисс, навязанный законами согласованности. Так что выбирайте свои типы тщательно!

    }

    def chapter12 = {
      // Резюме.

      // В то время как монады и функторы являются наиболее широко используемыми типами данных последовательности,
      // которые мы рассмотрели в этой книге,
      // полугруппы и аппликативные являются наиболее общими.
      // Эти классы типов предоставляют универсальный механизм для комбинирования значений и применения функций в контексте,
      // из которого мы можем формировать монады и множество других комбинаторов.

      // Semigroupal и Applicative чаще всего используются как средство объединения независимых значений,
      // таких как результаты правил проверки. Cats предоставляет Validated тип для этой конкретной цели,
      // наряду с синтаксисом apply как удобным способом выражения объединения правил.

      // Мы почти рассмотрели все концепции функционального программирования в нашей повестке дня для этой книги.
      // Следующая глава охватывает Traverse и Foldable,
      // два мощных класса типов для преобразования между типами данных.
      // После этого мы рассмотрим несколько тематических исследований, которые объединяют все концепции из Части I.

    }
  }

  part1.chapter1
  part1.chapter2
  part1.chapter3
  part1.chapter4
  part1.chapter5
  part1.chapter6
  part1.chapter7
  part1.chapter8
  part1.chapter9
  part1.chapter10
  part1.chapter11
  part1.chapter12



}


