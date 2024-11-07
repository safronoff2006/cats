


import cats._
import cats.data.{Reader, Writer, WriterT}
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

      // cats.data.Reader— это монада, которая позволяет нам упорядочивать операции, зависящие от некоторого ввода.
      // Экземпляры Reader обертывают функции одного аргумента, предоставляя нам полезные методы для их составления.

      // Одним из распространенных вариантов использования Readers является внедрение зависимости.
      // Если у нас есть несколько операций, которые зависят от некоторой внешней конфигурации,
      // мы можем связать их вместе, используя Reader чтобы создать одну большую операцию,
      // которая принимает конфигурацию в качестве параметра и запускает нашу программу в указанном порядке.

      final case class Cat(name: String, favoriteFood: String)

      // Мы можем создать Reader[A, B] из функции, A => B используя Reader.apply конструктор:
      val catName: Reader[Cat, String] = Reader(cat => cat.name)

      // Мы можем снова извлечь функцию, используя Reader's run метод, и вызвать ее, apply как обычно:
      val name: Id[String] = catName.run(Cat("Garfield", "lasagne"))
      println(name)

      // Комбинация Reader's
      // Сила Readers исходит из их методов map и flatMap, которые представляют собой различные виды композиции функций.
      // Обычно мы создаем набор Readers, которые принимают один и тот же тип конфигурации,
      // объединяем их с map и flatMap, а затем вызываем run для внедрения конфигурации в конце.

      //Метод map просто расширяет вычисление, Reader передавая его результат через функцию:

      val greetKitty: Reader[Cat, String] = catName.map(name => s"Hello ${name}")
      println {
        greetKitty.run(Cat("Heathcliff", "junk food"))
      }

      // Метод flatMap более интересен. Он позволяет нам объединять считыватели, которые зависят от одного и того же типа ввода.
      // Чтобы проиллюстрировать это, давайте расширим наш пример приветствия, чтобы также покормить кота:

      val feedKitty: Reader[Cat, String] =
        Reader(cat => s"Have a nice bowl of ${cat.favoriteFood}")

      val greetAndFeed: Reader[Cat, String] = for {
        greet <- greetKitty
        feed <- feedKitty
      } yield s"$greet. $feed."

      println {
        greetAndFeed(Cat("Garfield", "lasagne"))
      }
      println {
        greetAndFeed(Cat("Heathcliff", "junk food"))
      }

      // Упражнение: Хакинг Reader's

      // Классическое использование Readers— создание программ, которые принимают конфигурацию как параметр.
      // Давайте обоснуем это на полном примере простой системы входа.
      // Наша конфигурация будет состоять из двух баз данных: списка допустимых пользователей и списка их паролей:

      final case class Db(
                           usernames: Map[Int, String],
                           passwords: Map[String, String]
                         )

      // Начните с создания псевдонима типа DbReaderд ля a Reader,
      // который потребляет a Db в качестве входных данных.
      // Это сделает остальную часть нашего кода короче.

      type DbReader[A] = Reader[Db, A]

      // Теперь создайте методы, которые генерируют DbReaders для поиска имени пользователя для Int идентификатора пользователя
      // и поиска пароля для String имени пользователя.

      // Помните: идея заключается в том, чтобы оставить внедрение конфигурации напоследок.
      // Это означает настройку функций, которые принимают конфигурацию как параметр
      // и проверяют ее на соответствие конкретной информации пользователя, которую нам дали:

      def findUsername(userId: Int): DbReader[Option[String]] =
        Reader(db => db.usernames.get(userId))

      def checkPassword(username: String, password: String): DbReader[Boolean] =
        Reader(db => db.passwords.get(username).contains(password))

      // Наконец, создайте checkLogin метод для проверки пароля для заданного идентификатора пользователя.

      // Как и ожидалось, здесь мы используем flatMap для объединения в цепочку findUsername и checkPassword.
      // Мы используем pure для подъема Boolean до DbReader[Boolean], когда имя пользователя не найдено:

      import cats.syntax.applicative._ // for pure

      def checkLogin(userId: Int, password: String): DbReader[Boolean] = for {
        optUsername <- findUsername(userId)
        passwordOk <- optUsername.map { username => checkPassword(username, password) }
          .getOrElse {
            false.pure[DbReader]
          }
      } yield passwordOk

      val users = Map(
        1 -> "dade",
        2 -> "kate",
        3 -> "margo"
      )

      val passwords = Map(
        "dade" -> "zerocool",
        "kate" -> "acidburn",
        "margo" -> "secret"
      )

      val db = Db(users, passwords)

      println {
        checkLogin(1, "zerocool").run(db)
      }
      println {
        checkLogin(4, "davinci").run(db)
      }

      // Когда использовать ридеры?

      // Readers предоставляют инструмент для внедрения зависимости.
      // Мы записываем шаги нашей программы как экземпляры Reader,
      // связываем их вместе с mapи flatMap,
      // и создаем функцию, которая принимает зависимость в качестве входных данных.

      // Существует множество способов реализации внедрения зависимостей в Scala:
      // от простых методов, таких как метод с несколькими списками параметров, через неявные параметры и классы типов,
      // до сложных методов, таких как шаблон «торт» и фреймворки DI.

    }
  }

  object part2 {
    def part2 = {

    }
  }


  part1.part1
  part2.part2

}
