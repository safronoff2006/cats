


import cats.Eval
import cats.data.{IndexedStateT, State}

import scala.language.postfixOps


object Main extends App {

  println("Изучаем Cats!".toUpperCase)

  object part1 {
    def part1: Unit = {

      // В простейшей форме экземпляры State[S, A] представляют функции типа S => (S, A)
      // S — тип состояния, а A — тип результата.


      val a = State[Int, String] { state =>
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
    def part2 = {

      // Cила монады State заключается в объединении экземпляров.
      // Методы map и flatMap переносят состояние из одного экземпляра в другой.
      // Каждый отдельный экземпляр представляет собой атомарное преобразование состояния,
      // а их комбинация представляет собой полную последовательность изменений:

      val step1 = State[Int, String] { num =>
        val ans = num + 1
        (ans, s"Result of step1: $ans")
      }

      val step2 = State[Int, String] { num =>
        val ans = num * 2
        (ans, s"Result of step2: $ans")
      }

      val step3 = State[Int, String] { num =>
        val ans = num * 10
        (ans, s"Result of step3: $ans")
      }

      val both: IndexedStateT[Eval, Int, Int, String] = for {
        a <- step1
        b <- step2
        c <- step3
      } yield a + " # " + b + " # " + c

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
      println(state, result)
      println("---------------")
    }
  }

  object part5 {
    def part5 = {
      // Exercise: Post-Order Calculator

      // Монада State позволяет нам реализовывать простые интерпретаторы для сложных выражений,
      // передавая значения изменяемых регистров вместе с результатом.
      // Мы можем увидеть простой пример этого, реализовав калькулятор для выражений арифметики целых чисел в обратном порядке.

      // Если вы раньше не слышали о выражениях обратного порядка (не волнуйтесь, если не слышали),
      // это математическая нотация, в которой мы пишем оператор после его операндов.
      // Так, например, вместо того, чтобы писать 1 + 2, мы бы написали: 1 2 +

      // Хотя пост-порядковые выражения трудно читать людям, их легко оценить в коде.
      // Все, что нам нужно сделать, это пройти по символам слева направо, неся с собой стек операндов:

      // когда мы видим число, мы помещаем его в стек;
      // когда мы видим оператор, мы извлекаем два операнда из стека, выполняем над ними операции и помещаем результат на их место.

      // Это позволяет нам оценивать сложные выражения без использования скобок.
      // Например, мы можем оценить (1 + 2) * 3 следующим образом:

    /*
      1 2 + 3 * // see 1, push onto stack
      2 + 3 *   // see 2, push onto stack
      + 3 *     // see +, pop 1 and 2 off of stack,
                //        push (1 + 2) = 3 in their place
      3 3 *     // see 3, push onto stack
      3 *       // see 3, push onto stack
      *         // see *, pop 3 and 3 off of stack,
                //        push (3 * 3) = 9 in their place
     */

      // Давайте напишем интерпретатор для этих выражений.
      // Мы можем разобрать каждый символ на State экземпляр,
      // представляющий преобразование в стеке и промежуточный результат.
      // State Экземпляры можно объединить вместе,
      // используя flatMap для создания интерпретатора для любой последовательности символов.

      // Начните с написания функции evalOne, которая анализирует один символ в экземпляр State.
      // Используйте код ниже в качестве шаблона.
      // Не беспокойтесь об обработке ошибок на данный момент —
      // если стек находится в неправильной конфигурации, можно выбросить исключение.

      /*
      import cats.data.State

      type CalcState[A] = State[List[Int], A]

      def evalOne(sym: String): CalcState[Int] = ???
      */

      // Если это кажется сложным, подумайте о базовой форме State возвращаемых вами экземпляров.
      // Каждый экземпляр представляет собой функциональное преобразование из стека в пару стека и результата.
      // Вы можете игнорировать любой более широкий контекст и сосредоточиться только на этом одном шаге:

      /*
      State[List[Int], Int] { oldStack =>
        val newStack = someTransformation(oldStack)
        val result   = someCalculation
        (newStack, result)
      }
      */

      // Не стесняйтесь писать свои Stack экземпляры в этой форме
      // или в виде последовательностей удобных конструкторов, которые мы видели выше.

      // Требуемая операция стека отличается для операторов и операндов.
      // Для ясности мы реализуем evalOne в терминах двух вспомогательных функций, по одной для каждого случая:

      type CalcState[A] = State[List[Int], A]


      def evalOne(sym: String): CalcState[Int] = sym match {
          case "+" => operator(_ + _)
          case "-" => operator(_ - _)
          case "*" => operator(_ * _)
          case "/" => operator(_ / _)
          case num => operand(num.toInt)
      }

      // Давайте сначала посмотрим operand.
      // Все, что нам нужно сделать, это положить число в стек.
      // Мы также возвращаем операнд как промежуточный результат:

      def operand(num: Int): CalcState[Int] =
        State[List[Int], Int] { stack =>
          (num :: stack, num)
        }

      // Функция operator немного сложнее.
      // Нам нужно вытащить два операнда из стека (имея второй операнд наверху стека)
      // и поместить результат на их место.
      // Код может завершиться ошибкой, если в стеке недостаточно операндов,
      // но описание упражнения позволяет нам выдать исключение в этом случае:

      def operator(func: (Int, Int) => Int): CalcState[Int] =
        State[List[Int], Int] {
          case b :: a :: tail =>
            val ans = func(a, b)
            (ans :: tail, ans)

          case _ =>
            sys.error("Fail!")
        }

      // evalOne позволяет нам оценивать односимвольные выражения следующим образом.
      // Мы вызываем runA подставляя Nil как начальный стек и вызываем value
      // для распаковки полученного Eval экземпляра:

      println {
        evalOne("42").runA(Nil).value
      }

      // Мы можем представлять более сложные программы, используя evalOne, map и flatMap.
      // Обратите внимание, что большая часть работы происходит в стеке,
      // поэтому мы игнорируем результаты промежуточных шагов для evalOne("1") и evalOne("2"):

      val program = for {
        _   <- evalOne("1")
        _   <- evalOne("2")
        ans <- evalOne("+")
      } yield ans

      println {
        program.runA(Nil).value
      }

      // Обобщите этот пример, написав evalAll метод, который вычисляет результат List[String].
      // Используйте evalOne для обработки каждого символа и свяжите полученные Stateмонады вместе с помощью flatMap.
      // Ваша функция должна иметь следующую сигнатуру

      /*
        def evalAll(input: List[String]): CalcState[Int] = ???
      */

      // Мы реализуем evalAll путем сворачивания ввода.
      // Мы начинаем с чистого CalcState, который возвращает, 0 если список пуст.
      // Мы flatMap на каждом этапе игнорируем промежуточные результаты, как мы видели в примере:

      import cats.syntax.applicative._ // for pure

      def evalAll(input: List[String]): CalcState[Int] =
        input.foldLeft(0.pure[CalcState]) { (a, b) =>
          a.flatMap(_ => evalOne(b))
        }

      // evalAll Для удобной оценки многоступенчатых выражений мы можем использовать :

      val multistageProgram = evalAll(List("1", "2", "+", "3", "*"))

      println{
        multistageProgram.runA(Nil).value
      }

      // Поскольку evalOne и evalAll оба возвращают экземпляры State,
      // мы можем объединить эти результаты с помощью flatMap.
      // evalOne создает простое преобразование стека
      // и evalAll создает сложное, но они обе являются чистыми функциями,
      // и мы можем использовать их в любом порядке столько раз, сколько захотим:

      val biggerProgram = for {
        _   <- evalAll(List("1", "2", "+"))
        _   <- evalAll(List("3", "4", "+"))
        ans <- evalOne("*")
      } yield ans

      println {
        biggerProgram.runA(Nil).value
      }

      // Завершите упражнение, реализовав evalInput функцию,
      // которая разбивает входные данные String на символы,
      // вызывает evalAll и запускает результат с начальным стеком.

      // Теперь мы проделали всю тяжелую работу.
      // Все, что нам нужно сделать, это разделить входные данные на термы
      // и вызвать runA и value распаковать результат:

      def evalInput(input: String): Int =
        evalAll(input.split(" ").toList).runA(Nil).value

      println {
        evalInput("1 2 + 3 4 + *")
      }

      println {
        evalInput("7 2 - 5 * 4 *")
      }

    }
  }

  part1.part1
  part2.part2
  part3.part3
  part4.part4
  part5.part5


}
