



import cats._
import cats.data.Kleisli
import cats.effect.IO
import cats.effect.unsafe.implicits._
import cats.implicits._

import scala.concurrent.ExecutionContextExecutor
import scala.language.postfixOps


object logger {
  def info(msg: String): Unit = println(msg)
}

object Main extends App {

  println("Изучаем Cats!".toUpperCase)
  println("KLeisli")

  object part1 {
    def chapter1: Unit = {
      // Категория Kleisli — путешествие

      // В теории категорий категория Клейсли — это категория, естественным образом связанная с любой монадой T.

      // Пока все хорошо, но давайте разберем это на части, по ходу приводя несколько примеров,
      // чтобы мы могли вернуться к этому предложению в конце этого поста и понять, что именно оно означает.

      // Предположим, мы хотим обрабатывать журналы выполнения наших методов,
      // чтобы иметь возможность обрабатывать их как единый список журналов
      // (кстати, пример журнала, приводящий нас к категории, заимствован из замечательной серии видеолекций Бартоша Милевски).

      // https://www.youtube.com/watch?v=I8LbkfSSR58&list=PLbgaMIhjbmEnaH_LTkxLI7FMa2HsnawM_

      // Все примеры кода доступны в репозитории GitHub.
      // https://github.com/softwaremill/kleisli-example

      // Решение 1
      // Самым простым решением было бы хранить логи в глобальной переменной — я знаю, вы только что пожали плечами,
      // но не волнуйтесь, это всего лишь для демонстрационных целей.


      var logAcc: String = ""

      val simple_1: Boolean => Boolean = { x =>
        logAcc = logAcc + s"simple_1 called with ${x}\n"
        x
      }

      val simple_2: Boolean => Boolean = { x =>
        logAcc = logAcc + s"simple_2 called with ${x}\n"
        x
      }

      simple_1(true)
      simple_2(false)
      logger.info("LOG: \n" + logAcc)


      // С logAcc нашим накопителем журналов мы можем выполнять функции simple_1 и simple_2 хранить журналы в порядке.
      // Но... мы все знаем, что это ужасно, и мы все знаем, что если вы это читаете,
      // вы хотите увидеть какой-то функциональный способ решения проблем :)

      println("----------------------")
    }

    def chapter2 = {
      // Решение 2

      // Второй подход — мы передаем логи от одного выполнения функции к другому в качестве параметра:

      val negate_1: (Boolean, String) => (Boolean, String) = { (x: Boolean, log: String) =>
        (!x, log + s"negate_1 called with ${x}\n")
      }

      val negate_2: (Boolean, String) => (Boolean, String) = { (x: Boolean, log: String) =>
        (!x, log + s"negate_2 called with ${x}\n")
      }

      val negate_1_Result = negate_1(true, "")
      val negate_2_Result = negate_2(false, negate_1_Result._2)
      logger.info("LOG: \n" + negate_2_Result._2)

      // Это лучше, функции чистые и, по крайней мере, мы знаем из сигнатуры,
      // что отрицающая функция тоже что-то делает.
      // Мы можем передать второй элемент результирующего кортежа следующей функции
      // и получить наш объединенный журнал в конце.
      // Это, конечно, плохо, функция использует некоторую логику для обработки журналов внутри,
      // что нарушает принцип единой ответственности, и функцию трудно запомнить,
      // что должно включать красный свет, когда думаешь о чистых функциях.
      // Давайте попробуем что-нибудь еще.

      println("----------------------")

    }

    def chapter3 = {
      //Мы можем решить вернуть кортеж, как мы сделали во втором примере,
      // но мы берем только Boolean параметр.
      // Мы откладываем конкатенацию журнала до точки, где мы вызываем наши функции:

      val negate_1: Boolean => (Boolean, String) = { (x: Boolean) =>
        (!x, s"negate_1 called with ${x}\n")
      }

      val negate_2: Boolean => (Boolean, String) = { (x: Boolean) =>
        (!x, s"negate_2 called with ${x}\n")
      }

      val negate_1_Result = negate_1(true)
      val negate_2_Result = negate_2(false)
      logger.info("LOG: \n" + negate_1_Result._2 + negate_2_Result._2)

      // Теперь у нас есть чистые функции и некоторый 'контекст' журнала,
      // который мы передаем и обрабатываем в конце.
      // Все хорошо, за исключением способа, которым мы обрабатываем полученный журнал,
      // нам нужно сохранять результаты каждого выполнения, объединять журналы и т. д., это может быть сложно.
      // Другими словами, нам нужен лучший способ составления этих функций.

      println("----------------------")
    }

    def chapter4 = {

      // Решение 4

      // С помощью композиции функций мы берем две компонуемые функции и создаем из них одну функцию.
      // Если вы интересуетесь теорией категорий, вы, вероятно, уже видели пример композиции
      // function A -> B с function B -> C, что приводит к function A -> C.
      // Композиция имеет основополагающее значение для функционального программирования.
      // Имея три типа A, B, C мы можем создать нашу compose функцию следующим образом:

      def compose[A, B, C](f: A => B, g: B => C): A => C = { (x: A) => {
        val p1 = f(x)
        val p2 = g(p1)
        p2
      }
      }

      // Функция выше принимает две функции в качестве аргументов и возвращает еще одну функцию типа A -> C.

      // С нашим Tuple примером и 'контекстом журнала' мы можем создать собственную составную функцию.
      // Функция будет принимать две функции, принимающие Boolean и возвращающие ( Boolean, String),
      // и она преобразует их в одну функцию, которая принимает Boolean и возвращает ( Boolean, String).
      // Для этого примера я обобщил Boolean до универсального типа A:

      def composeT[A](f: A => (A, String),
                      g: A => (A, String)): A => (A, String) =
      { (x: A) =>
        {
          val p1 = f(x)
          val p2 = g(p1._1)
          (p2._1, p1._2 + p2._2) //string appending happens inside the composition,
                                  // I'm combining results of 2 functions, this is my way of composing functions
        }
      }

      val negate_1: Boolean => (Boolean, String) = { (x: Boolean) =>
        (!x, s"negate_1 called with ${x}\n")
      }

      val negate_2: Boolean => (Boolean, String) = { (x: Boolean) =>
        (!x, s"negate_2 called with ${x}\n")
      }

      def composed: Boolean => (Boolean, String) = composeT(negate_1, negate_2)

      logger.info(s"Result: ${composed.apply(true)}")

      // Обратите внимание, что операция конкатенации строк происходит внутри нашей композиции — composeT функции.
      // Мы знаем, глядя на нашу функцию, что это простая композиция функций и она ассоциативна,
      // и имея некоторые знания о String, мы знаем, что composeT должна быть ассоциативной,
      // поскольку String конкатенация ассоциативна.

      // Если у нас есть ассоциативность, давайте проверим, можем ли мы создать идентичность.
      // Чтобы создать функцию идентичности, нам нужна функция, которая ничего не сделает с нашим журналом.
      // Другими словами, нам нужна функция, которая будет принимать Boolean и возвращать то же Boolean значение
      // с пустым String в качестве кортежа:

      def id[A](a: A): (A, String) = (a, "")

      println("----------------------")
    }

    def chapter5 = {
      // Ассоциативность? Идентичность? Вам это ничего не напоминает?

      // На этом этапе мы знаем, что у нас есть ассоциативность и идентичность —
      // другими словами, у нас есть Категория.
      // В нашей Категории объекты являются типами Scala, а стрелки A -> (B, String) (вместо простых стрелок A -> B).
      // Тот факт, что стрелки не являются простым преобразованием из A -> B, делает их так называемыми стрелками Клейсли.
      // Стрелки Клейсли могут работать со многими различными типами, а не только с кортежем некоторого типа B и String,
      // они определены для типов, на которые мы накладываем как можно меньше условий, другими словами, для монадических типов.

      // Вот где вступает в действие определение Клейсли от Cats :

      // Kleisli позволяет создавать композиции функций, возвращающих монадическое значение,
      // например, Option[Int] или Either[String, List[Double]],
      // без необходимости принимать Option или Either качестве параметра, что может быть странным и громоздким.

      // Итак, чем это полезно?
      // Kleisli — это все о композиции. Допустим, у нас есть 3 функции, которые мы хотим скомпоновать:

      val r = scala.util.Random


      val generate: Unit => Int = _ => r.nextInt(100)
      val process: Int => Int = v => (v * math.Pi).toInt
      val save: Int => Boolean = _ => true

      // Самый простой подход — вызывать их по одному и передавать результат от одного к другому следующим образом:

      val generated: Int = generate(())
      val processed: Int = process(generated)
      val saved: Boolean = save(processed)

      logger.info(s"Result is: $saved")

      // Или вы можете просто встроить эти вызовы следующим образом:




      val combine_1: Unit => Boolean = _ => save(process(generate(())))
      logger.info(s"Result 1 is: ${combine_1(())}")

      // В Scala для этого есть такие функции, как compose и andThen


      val combine_2: Unit => Boolean = save compose process compose generate
      logger.info(s"Result 2 is: ${combine_2(())}")
      //this is a bit difficult to read too as we are used to read from left to right


      //andThen version

      val combine_3: Unit => Boolean = generate andThen process andThen save
      logger.info(s"Result 3 is: ${combine_3(())}")

      // Проблема со всеми вышеприведенными примерами заключается в том,
      // что нам нужно сопоставить входы одной функции с выходами другой, чтобы все это работало.
      // Если некоторые из выходов будут оберткой вокруг какого-то типа
      // (например, Future или Cats IO), мы быстро попадем в беду.

      // Давайте преобразуем некоторые значения во что-то полезное,
      // чтобы увидеть, как вы обычно с этим справляетесь:

      val generateIO: Unit => IO[Int] = _ => IO.pure(r.nextInt(100))
      val processIO: Int => IO[Double] = num => IO.pure(num * math.Pi)
      val saveIO: Double => IO[Boolean] = number => IO.pure(true)

      // Теперь ясно, что мы не можем просто передать возвращаемое значение из generate в process и т. д.
      // Но мы знаем, что IO — это монада, и мы можем легко применить к ней flatMap:
      val flatMappedVersion: Unit => Boolean = _ => {
        val comboFlatMap: Unit => IO[Boolean] = _ => {
          generateIO(()).flatMap { number =>
            processIO(number).flatMap { processed =>
              saveIO(processed)
            }
          }
        }
        comboFlatMap(()).unsafeRunSync()
      }

      logger.info(s"FlatMap that sh**t: ${flatMappedVersion(())}")

      // или с более чистым подходом — для понимания:

      val forComp: Unit => Boolean = _ => {
        val comboForComp: Unit => IO[Boolean] = _ => {
          for {
            number <- generateIO(())
            processed <- processIO(number)
            result <- saveIO(processed)
          } yield result
        }
        comboForComp(()).unsafeRunSync()
      }
      logger.info(s"For comprehension version: ${forComp(())}")

      println("----------------------")

      // С Kleisli мы можем сделать что-то похожее, но во многих случаях более понятным и читабельным способом.
      // Давайте начнем с самой подробной версии:

      val kleisliCombine_1: Kleisli[IO, Unit, Boolean] = {
        val generateK: Kleisli[IO, Unit, Int] = Kleisli(generateIO)
        val processK: Kleisli[IO, Int, Double] = Kleisli(processIO)
        val saveK:Kleisli[IO, Double, Boolean] = Kleisli(saveIO)
        generateK andThen processK andThen saveK
      }
      logger.info(s"Kleilis example 1: ${kleisliCombine_1.run(()).unsafeRunSync()}")

      // Здесь мы оборачиваем каждую функцию в Kleisli(..) тип,
      // а затем используем тот же andThen механизм, который мы использовали с функциями,
      // сопоставляющими входы и выходы
      // (Примечание: библиотека Cats фактически переопределяет Scala andThen,
      // и Scalaz использует ее andThenK для этой цели).

      // Пока все хорошо, но мы можем сделать лучше :).
      // В Cats есть оператор для работы с Kleisli, который можно использовать для получения более чистого кода:

      val kleisliCombine_2: Kleisli[IO, Unit, Boolean] = Kleisli(generateIO) >>> Kleisli(processIO) >>> Kleisli(saveIO)
      logger.info(s"Kleilis example 2: ${kleisliCombine_2.run(()).unsafeRunSync()}")

      // Кроме того, если вы используете andThen подход, вам не нужно оборачивать Kleisli все функции,
      // которые вы хотите использовать. Достаточно первой, например:

      val kleisliCombine_3: Kleisli[IO, Unit, Boolean] = Kleisli(generateIO) andThen processIO andThen saveIO
      logger.info(s"Kleilis example 3: ${kleisliCombine_3.run(()).unsafeRunSync()}")

      // Надеюсь, что в этом уроке вы смогли понять, почему категория Kleisli является категорией,
      // естественным образом связанной с любой монадой T :).
      // Мы рассмотрели простой пример с кортежем, чтобы перейти к категории и стрелкам Kleisli,
      // а от этого — к определению и использованию библиотеки Cats.

      // Наслаждаться!

      println("----------------------")

    }
  }

  part1.chapter1
  part1.chapter2
  part1.chapter3
  part1.chapter4
  part1.chapter5
}


