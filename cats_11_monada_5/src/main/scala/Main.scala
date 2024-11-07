


import cats.Eval
import cats.data.{IndexedStateT, State}

import scala.language.postfixOps


object Main extends App {

  println("Изучаем Cats!".toUpperCase)

  object part1 {
    def part1: Unit = {

      // В простейшей форме экземпляры State[S, A] представляют функции типа S => (S, A)
      // S — тип состояния, а A — тип результата.


      val a = State[Int, String]{ state =>
        (state, s"The state is $state")
      }

      // экземпляр State — это функция, которая выполняет две вещи:
      // преобразует входное состояние в выходное состояние;
      // вычисляет результат.


      // Мы можем «запустить» нашу монаду, указав начальное состояние.
      // State предоставляет три метода run runS и runA — которые возвращают различные комбинации состояния и результата.
      // Каждый метод возвращает экземпляр Eval, который State использует для поддержания безопасности стека.
      // Мы вызываем value метод как обычно, чтобы извлечь фактический результат:

      val (state, result) = a.run(10).value
      println(state, result)

      val justTheState = a.runS(10).value
      println(justTheState)

      val justTheResult = a.runA(10).value
      println(justTheResult)

      println("---------------")


    }
  }

  object part2 {
    def part2= {

      // Cила монады State заключается в объединении экземпляров.
      // Методы map и flatMap переносят состояние из одного экземпляра в другой.
      // Каждый отдельный экземпляр представляет собой атомарное преобразование состояния,
      // а их комбинация представляет собой полную последовательность изменений:

      val step1 = State[Int, String]{ num =>
        val ans = num + 1
        (ans, s"Result of step1: $ans")
      }

      val step2 = State[Int, String]{ num =>
        val ans = num * 2
        (ans, s"Result of step2: $ans")
      }

      val step3 = State[Int, String]{ num =>
        val ans = num * 10
        (ans, s"Result of step3: $ans")
      }

      val both: IndexedStateT[Eval, Int, Int, String] = for {
        a <- step1
        b <- step2
        c <- step3
      } yield a + " # " +  b + " # " + c

      val (state, result) = both.run(20).value
      println(state, result)

      // Как вы можете видеть, в этом примере конечное состояние является
      // результатом применения обоих преобразований последовательно.
      // Состояние передается от шага к шагу, хотя мы не взаимодействуем с ним в for comprehension.

      println("---------------")

    }
  }

  object part3 {
    def part3 = {

      // Общая модель использования State монады заключается в представлении каждого шага вычисления как экземпляра
      // и составлении шагов с использованием стандартных операторов монады.
      // Cats предоставляет несколько удобных конструкторов для создания примитивных шагов:

      // get извлекает состояние как результат;
      // set обновляет состояние и возвращает единицу измерения в качестве результата;
      // pure игнорирует состояние и возвращает предоставленный результат;
      // inspect извлекает состояние с помощью функции преобразования;
      // modify обновляет состояние с помощью функции обновления.

      val getDemo: State[Int, Int] = State.get[Int]
      println {
        getDemo.run(10).value
      }

      val setDemo: State[Int, Unit] = State.set[Int](30)
      println {
        setDemo.run(10).value
      }

      val pureDemo: State[Int, String] = State.pure[Int, String]("Result")
      println {
        pureDemo.run(10).value
      }

      val inspectDemo = State.inspect[Int, String](x => s"${x}!")
      println {
        inspectDemo.run(10).value
      }

      val modifyDemo = State.modify[Int](_ + 1)
      println {
        modifyDemo.run(10).value
      }

      println("---------------")

    }
  }

  object part4 {
    def part4 = {
      import State._

      val program: State[Int, (Int, Int, Int)] = for {
        a <- get[Int]
        _ <- set[Int](a + 1)
        b <- get[Int]
        _ <- modify[Int](_ + 1)
        c <- inspect[Int, Int](_ * 1000)
      } yield (a, b, c)

      val (state, result) = program.run(1).value
      println(state,result)
      println("---------------")
    }
  }

  part1.part1
  part2.part2
  part3.part3
  part4.part4



}
