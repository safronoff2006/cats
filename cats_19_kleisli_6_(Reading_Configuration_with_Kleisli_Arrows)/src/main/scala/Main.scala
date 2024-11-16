import cats.{Apply, Monad}
import cats.data.Kleisli
import cats.implicits._

import scala.language.postfixOps


object Main extends App {

  println("Изучаем Cats!".toUpperCase)
  println("KLeisli")

  object part1 {

    final case class Name(first: String, last: String)

    final case class Age(age: Int)

    final case class Person(name: Name, age: Age)

    final case class Config(name: String, age: Int)

   def chapter1 = {
     // Чтение конфигурации с помощью стрелок Клейсли

     // В предыдущей статье мы рассмотрели, как стрелки Клейсли составляют функции в монадическом контексте.
     // http://sanj.ink/posts/2017-06-07-composing-monadic-functions-with-kleisli-arrows.html

     // Стрелка Клейсли определяется следующим образом:

     /*
          Kleisli[F[_], A, B](run: A => F[B])
     */

     // resources/kleisli-type.jpg

     // По сути, он оборачивает функцию:
     // A => F[B]
     // При наличии некоторого A он возвращает результат B в контексте F.

     // Reader and ReaderT

     // Если мы посмотрим на сигнатуру монады Reader, то увидим, что она оборачивает несколько похожую функцию:
     // A => B
     // При наличии некоторого A он возвращает B без какого-либо контекста.

     // resources/reader.jpg

     // Трансформер монады ReaderT оборачивает ту же функцию, что и стрелка Клейсли:
     // http://hackage.haskell.org/package/mtl-2.2.1/docs/Control-Monad-Reader.html#v:ReaderT

     /*
          ReaderT[F[_], A, B](A => F[B])
     */

     // При наличии некоторого A он возвращает B в контексте F.

     // resources/readert-type.jpg

     // Стрелка Клейсли имеет ту же форму (изоморфна) монаде ReaderT. Но каково ее отношение к монаде Reader?
     // Конструктор типа Id аналогичен функции идентификации, за исключением того,
     // что он возвращает предоставленный ему тип, а не значение:

     /*
             type Id[A] = A // returns the type supplied
             def identity[A](value: A): A = value //returns the value supplied
     */
     // resources/id-type.jpg

     // Вооружившись конструктором типа Id ,мы можем определить монаду Reader
     // в терминах монады ReaderT (и стрелки Клейсли):

     /*
           ReaderT[F[_], A, B]  ==
           ReaderT[Id, A, B]    == //specialising for Id
           ReaderT(A => Id[B])  ==
           Reader(A => B)       == //since Id[B] == B
           Kleisli(A => B)         //since ReaderT == Kleisli
     */

     // В Cats монады Reader и ReaderT определяются в терминах стрелки Клесли:

     /*
           type Reader[A, B]        = Kleisli[Id, A, B]
           type ReaderT[F[_], A, B] = Kleisli[F, A, B]
     */

     // Использование Kleisli Arrows как ReaderT

     // Давайте попробуем использовать стрелку Клейсли для чтения некоторой конфигурации из среды,
     // чтобы получить что-то полезное. В следующем примере мы хотим создать объект Person из Name и Age,
     // полученных из объекта Config, который содержит оба значения:

     // Создание Имени и Возраста имеет свои правила и может завершиться неудачей,
     // если правила не соблюдаются:

     def readName: Config => Option[Name] = c => {
       val parts = c.name.split(" ")
       if (parts.length > 1) Option(Name(parts(0), parts.drop(1).mkString(" "))) else None
     }

     def readNameK: Kleisli[Option, Config, Name] = Kleisli(readName)

     def readAge: Config => Option[Age] = c => {
       val age = c.age
       if (age >= 1 && age <= 150) Option(Age(age)) else None
     }

     def readAgeK: Kleisli[Option, Config, Age] = Kleisli(readAge)

     // readNameK и readAgeK требуют объект Config для извлечения своих значений
     // и обернуты в стрелку Kleisli.
     // Стрелка Kleisli должна предоставить тот же объект Config обеим функциям.
     // Это существенно отличается от композиции Kleisli, где вывод из одной функции подавался в следующую.
     // В этом случае между двумя функциями нет композиции.

     // Как бы мы объединили эти функции?
     // Поскольку стрелки Клейсли отображают функции в монадическом контексте:
     // Kleisli[F[_], A, B] //F has a Monad instance
     // мы можем использовать for-comprehension для решения нашей проблемы:

     val personK: Kleisli[Option, Config, Person] =
       for {
         name <- readNameK
         age  <- readAgeK
       } yield Person(name, age)

     //Some(Person(Name(Bojack,Horseman),Age(42)))
     val result1 = personK(Config("Bojack Horseman", 42))

     //None - Name is not space-separated
     val result2 = personK(Config("Jake", 20))

     //None - age is not between 1 and 150
     val result3 = personK(Config("Fred Flintstone", 50000))

     println(
       result1,
       result2,
       result3
     )

     println("-------------------------")


   }

   def chapter2 = {
     // Использование аппликаторов для чтения конфигурации

     def readName: Config => Option[Name] = c => {
       val parts = c.name.split(" ")
       if (parts.length > 1) Option(Name(parts(0), parts.drop(1).mkString(" "))) else None
     }

     def readNameK: Kleisli[Option, Config, Name] = Kleisli(readName)

     def readAge: Config => Option[Age] = c => {
       val age = c.age
       if (age >= 1 && age <= 150) Option(Age(age)) else None
     }

     def readAgeK: Kleisli[Option, Config, Age] = Kleisli(readAge)


     // Вы могли заметить, что функция readAgeK напрямую не зависит от вывода readNameK.
     // Это означает, что нам не нужно использовать здесь Monad (для понимания)
     // и можно использовать что-то немного менее мощное, например Apply.
     // Класс типов Apply является Applicative без чистой функции.
     // Тип данных Kleisli имеет экземпляр для Apply со следующей сигнатурой:

     /*
           Apply[Kleisli[F, A, ?]]
     */

     // Давайте попробуем переписать фрагмент кода с Apply вместо этого  Monadic.
     // Мы можем использовать функцию ap2 , которая имеет следующее определение:

     /*
            def ap2[A, B, Z](ff: F[(A, B) => Z])(fa: F[A], fb: F[B]): F[Z]
     */

     // Используя ap2, мы можем создать экземпляр Person следующим образом:

     type KOptionConfig[A] = Kleisli[Option, Config, A]
     type PersonFunc = (Name, Age) => Person

     val config1 = Config("mr peanutbutter", 30)
     val readNameKOC: KOptionConfig[Name] = readNameK
     val readAgeKOC: KOptionConfig[Age] = readAgeK
     val personKOC: KOptionConfig[PersonFunc] = Kleisli { _:Config => Option(Person(_: Name, _: Age)) }

     //Kleisli[Option, Config, Person]
     val personK1: KOptionConfig[Person] = Apply[KOptionConfig].ap2(personKOC)(readNameKOC, readAgeKOC)

     //Some(Person(Name(mr,peanutbutter),Age(30)))
     println {
       personK1(config1)
     }
     // Это решение, хотя и «менее мощное», чем версия Monadic, выглядит в Scala несколько уродливее
     // из-за приписывания типов отдельным функциям.
     // Мы также можем сделать нечто очень похожее, используя метод map2 :
     /*
          def map2[A, B, Z](fa: F[A], fb: F[B])(f: (A, B) => Z): F[Z]
     */
     // что может быть проще для рассуждений, чем ap2 , но по сути они достигают того же результата:

     val config2 = Config("Diane Nguyen", 27)

     //Kleisli[Option, Config, Person]
     val personK2 = Apply[KOptionConfig].map2(readNameKOC, readAgeKOC) { Person(_: Name, _: Age) }

     println{
       personK2(config2)
     }
     println("------------------------")
   }

   def chapter3 = {
     // Стрелки Клейсли с различными входами



     // Следует отметить, что мы продолжаем выравнивать тип ввода, в данном случае Конфигурацию,
     // по всем стрелкам Клейсли.

     // Как бы мы объединили стрелки Клейсли с различными типами ввода?

     // Здесь в игру вступает локальная функция. Она определяется как:
     // https://github.com/typelevel/cats/blob/155f7f534993c30d6e757de990330ac796dad5da/core/src/main/scala/cats/data/Kleisli.scala#L48

     /*
          def local[AA](f: AA => A): Kleisli[F, AA, B]
     */
     // resources/kleisli-local.jpg

     // По сути, он преобразует  Kleisli[F, A, B] в  Kleisli[F, AA, B],
     // если мы предоставим ему функцию для преобразования AA в A.
     // Функция f здесь преобразует некоторый другой тип входных данных AA в наш требуемый тип входных данных A. Это позволяет нам комбинировать стрелки Клейсли с различными типами входных данных в качестве локальной функции, расширяя тип входных данных до каждой стрелки Клейсли как AA .

     // Давайте перепишем наш предыдущий пример со стрелками Клейсли,
     // которые принимают строку в качестве входных данных для функции readName и целое число
     // в качестве входных данных для функции readAge :

     def readName: String => Option[Name] = name => {
       val parts = name.split(" ")
       if (parts.length > 1) Option(Name(parts(0), parts.drop(1).mkString(" "))) else None
     }

     def readAge: Int => Option[Age] = age => {
       if (age >= 1 && age <= 150) Option(Age(age)) else None
     }

     def readNameK: Kleisli[Option, String, Name] = Kleisli(readName)

     def readAgeK: Kleisli[Option, Int, Age] = Kleisli(readAge)

     // Затем мы расширяем типы входных данных с помощью локальной функции,
     // которая принимает объект Config :

     val personK1: Kleisli[Option, Config, Person] =
       for {
         name <- readNameK.local[Config](_.name)
         age  <- readAgeK.local[Config](_.age)
       } yield Person(name, age)

     println(
       //Some(Person(Name(Bojack,Horseman),Age(42)))
       personK1(Config("Bojack Horseman", 42)),

       //None
       personK1(Config("Jake", 20)),

       //None
       personK1(Config("Fred Flintstone", 50000))
     )

     // И используя map2 мы получаем те же результаты:
     type KOptionConfig[A] = Kleisli[Option, Config, A]

     val config = Config("Diane Nguyen", 27)
     val readNameKOC: KOptionConfig[Name] = readNameK.local[Config](_.name)
     val readAgeKOC: KOptionConfig[Age] = readAgeK.local[Config](_.age)

     val personK2 = Apply[KOptionConfig].map2(readNameKOC, readAgeKOC) { Person(_: Name, _: Age) }

     println {
       personK2(config)
     }

   }
  }

  part1.chapter1
  part1.chapter2
  part1.chapter3

}


