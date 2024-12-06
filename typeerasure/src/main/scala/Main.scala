
object Main extends App {
  println("Type Erasure Lesson!")

  object part1 {

    case class Thing[T](value: T)



    def chapter1(): Unit = {

      def processThing(thing: Thing[_]) = {
        thing match {
          case Thing(value: Int) => "Thing of int"
          case Thing(value: String) => "Thing of string"
          case _ => "Thing of something else"
        }
      }

      println(processThing(Thing(1)))
      println(processThing(Thing("hello")))

      println("-----------")

    }

    def chapter2(): Unit = {

       /*

       def processThing(thing: Thing[_]) = {
          thing match {
            case Thing(value: Int) => "Thing of int"
            case Thing(value: String) => "Thing of string"
            case Thing(value: Seq[Int]) => "Thing of Seq[int]"
            case _ => "Thing of something else"
          }
        }

        println(processThing(Thing(1)))
        println(processThing(Thing("hello")))
        println(processThing(Thing(Seq(5))))
        println(processThing(Thing(Seq("eded"))))
        println(processThing(Thing(Some(1))))

        */



      println("-----------")
    }

    def chapter3() = {

      import scala.reflect.runtime.universe._


      def processThing[T: TypeTag](thing: Thing[T]) = {
        typeOf[T] match {
          case t if t =:= typeOf[Seq[Int]] => "Thing of Seq[Int]"
          case t if t =:= typeOf[Seq[String]] => "Thing of Seq[String]"
          case t if t =:= typeOf[Int] => "Thing of Int"
          case _ => "Thing of other"
        }
      }
      println(processThing(Thing(1)))
      println(processThing(Thing("hello")))
      println(processThing(Thing(Seq(5))))
      println(processThing(Thing(Seq("eded"))))
      println(processThing(Thing(Some(1))))

      println("-----------")

      def processThing2[T : TypeTag](thing: Thing[T]) = {
        thing match {
          case Thing(value: Int) => "Thing of int " + value.toString
          case Thing(value: Seq[Int] @unchecked) if typeOf[T] =:= typeOf[Seq[Int]] => "Thing of seq of int " + value.sum
          case _ => "Thing of something else"
        }
      }

      println(processThing2(Thing(1)))
      println(processThing2(Thing("hello")))
      println(processThing2(Thing(Seq(5,5,5))))

      println("-----------")

      // Стоит отметить, что это действительно проблема только потому,
      // что тип Thing[T] совершенно не ограничен.
      // Если бы вы заранее знали, какие возможные типы вы можете вставить в Thing,
      // вы могли бы разработать другой тип с подтипами, которые вы могли бы сопоставить вместо этого:

      sealed trait ThingValue
      case class SeqIntThingValue(value: Seq[Int]) extends ThingValue
      case class SeqStringThingValue(value: Seq[String]) extends ThingValue

      def processThing3[T <: ThingValue](thing: Thing[T]) = {
        thing match {
          case Thing(SeqIntThingValue(value: Seq[Int])) => "Seq of Int " + value.sum
          case Thing(SeqStringThingValue(value: Seq[String])) => "Seq of String " + value.toString()
          case _ => "Other thing"
        }
      }

      println(processThing3(Thing(SeqIntThingValue(Seq(1,2,3)))))

      println(processThing3(Thing(SeqStringThingValue(Seq("hello","world")))))

      println(processThing3(Thing(SeqIntThingValue(Seq(5,5,5)))))

      println("-----------")
    }

  }

  object part2 {

    class Foo

    class Bar extends Foo

    def chapter1() = {
      println("==========================================")

      // Есть 3 вида тегов типов:

      // TypeTag;
      // ClassTag;
      // WeakTypeTag.

      // TypeTag и WeakTypeTag очень похожи, в то время как ClassTag это принципиально другая конструкция.

      /*

          object ListUtils {
            def retrieve[T](list: List[Any]) =
              list.flatMap {
                case element: T => Some(element)
                case _ => None
              }
          }

      */

      // Из описанного ранее понятно, что такой код работать не будет.
      // Для того чтобы исправить это, можно просто добавить дополнительный неявный параметр:
      import scala.reflect.ClassTag

      object ListUtils1 {
        def retrieve[T](list: List[Any])(implicit tag: ClassTag[T]): List[T] =
          list.flatMap {
            case element: T => Some(element)
            case _ => None
          }
      }

      val list: List[Any] = List(3, 10, "string", List(), "anotherString")
      val result1 = ListUtils1.retrieve[String](list)
      println(result1) // List(string, anotherString)

      // Для достижения того же самого результата можно также воспользоваться синтаксисом "context bound".
      // Сontext bound - это синтаксический сахар для списка неявных параметров
      // (можно указывать один или несколько типов через ":").
      // То есть, следующее объявление метода ListUtils.retrieve будет аналогично приведенному выше:
      object ListUtils2 {
        def retrieve[T: ClassTag](list: List[Any]) =
          list.flatMap {
            case element: T => Some(element)
            case _ => None
          }
      }

      val result2 = ListUtils2.retrieve[String](list)
      println(result2) // List(string, anotherString)

      // ClassTag

      // ClassTag предназначен для хранения частичной информации о Scala-типе.
      // Он содержит информацию только о том классе, чья информация стирается.
      // То есть, предоставляет доступ только к runtime-типу класса.
      // Например, ClassTag[List[String]] будет содержать информацию только о типе List,
      // без подробностей, что это List[String].

      //  Как все это работает? Когда указан неявный параметр с типом ClassTag,
      //  компилятор создает его для нас. Вот, что говорится в документации:
      // If an implicit value of type u.ClassTag[T] is required, the compiler will make one up on demand.

      // Такой же механизм действует для TypeTag и WeakTypeTag.

      // Код, приведенный выше, заработает, потому что компилятор перепишет наше условие
      // (так как в области видимости есть неявный тег):

      // case element: T => Some(element)
      // на
      // case (element @ tag(_: T)) => Some(element)

      // Конструкция "@" позволяет получить ссылку на объект совпавшего класса. Например:

      // case Foo(p, q) => // получаем только параметры в переменных p и q
      // case f @ Foo(p, q) => // получаем весь объект класса Foo через f + параметры в p и q

      // Если для типа T нет неявного тега ClassTag, то компилятор выдаст предупреждение о том,
      // что наше сопоставление с образцом не будет работать из-за стирания информации о типе T.

      // Как было упомянуто выше, особенность типа ClassTag заключается в том,
      // что он хранит только частичную информацию и предоставляет доступ только к runtime-типу класса.
      // То есть, мы не сможем отличать типы на более высоких уровнях (можем только на первом уровне).
      // Например, в списке List[List[Any]], мы не сможем отличить List[Int] от List[String].
      // Для решения данной задачи необходим TypeTag.

      println("---------")
    }

    def chapter2() = {

      // TypeTag
      // TypeTag предназначен для хранения полного описания Scala-типа.
      // С помощью него можно получить информацию о типе любой вложенности -
      // например, как о List[Any], так и о List[Set[Any]].

      // Для получения полной информации о типе, можно вызвать метод TypeTag.tpe.
      // Рассмотрим пример, из которого видно, что этот метод возвращает в том числе и ту информацию,
      // которой не было в ClassTag:

      import scala.reflect.runtime.universe._

      object Utils {
        def retrieve[T](x: T)(implicit tag: TypeTag[T]): String =
          tag.tpe match {
            case TypeRef(utype, usymbol, args) => List(utype, usymbol, args).mkString("\n")
          }
      }


      val list: List[Int] = List(1, 2)
      val result = Utils.retrieve(list)
      println(result)

      // Выведет:
      //   scala.type
      //   type List
      //   List(Int) <- информации об Int в ClassTag нет

      // Недостаток тега TypeTag заключается в том, что он не может быть использован с объектами в runtime.
      // Если мы заменим в приведенном ранее методе ListUtils.retrieve класс ClassTag на TypeTag,
      // то он перестанет работать.

      // С помощью TypeTag мы можем получить информацию об определенном типе в runtime,
      // но мы не сможем получить с помощью него тип какого-либо объекта в runtime.
      // Например, если мы будем передавать в метод объект List(1, 2) и укажем у этого параметра тип List[Any],
      // то TypeTag покажет, что мы передали List[Any].

      // Другими словами, TypeTag дает нам информацию о runtime типе, а
      // ClassTag дает нам информацию о том, какой на самом деле тип (верхнеуровневый) у определенного объекта в runtime.

      // Еще одно небольшое отличие TypeTag от ClassTag заключается в том,
      // что мы должны импортировать его из корректного объекта universe (в случае TypeTag, из scala.reflect.runtime.universe).
      // В то время как ClassTag c universe никак не связан.

      // Universe provides a complete set of reflection operations
      // which make it possible for one to reflectively inspect Scala type relations,
      // such as membership or subtyping.



      println("---------")
    }

    def chapter3() = {

      // WeakTypeTag

      //TypeTag не позволяет получить тип, если этот тип абстрактный.
      // Для таких задач используется WeakTypeTag[T], который обобщает TypeTag[T].
      // Он предназначен для хранения описания абстрактных типов.
      // В отличие от TypeTag, компоненты WeakTypeTag могут ссылаться на параметры типа или абстрактные типы.

      import scala.reflect.runtime.universe._

      abstract class MyClass[T] {

        object Utils {
          def retrieve[T](x: T)(implicit tag: WeakTypeTag[T]): String =
            tag.tpe match {
              case TypeRef(utype, usymbol, args) =>
                List(utype, usymbol, args).mkString("\n")
            }
        }

        val list: List[T]
        val result: String = Utils.retrieve(list)
        println(result)
      }

      new MyClass[Int] {
        val list: List[Int] = List(1)
      }

      // Выведет:
      //   scala.type
      //   type List
      //   List(T)

      // Полученный тип - List[T].
      // Если бы вместо WeakTypeTag мы использовали TypeTag, компилятор выдал бы сообщение "no TypeTag available for List[T]".
      // Однако, WeakTypeTag старается быть как можно более конкретным: если для абстрактного типа доступен тег типа,
      // WeakTypeTag использует его и сделает тип конкретным, а не абстрактным.

      println("---------")
    }

    def chapter4() = {

      // Альтернативный способ получения тегов типов

      // Наряду с возможностью получить теги типов через неявные параметры,
      // есть возможность воспользоваться для их получения специальными хелперами:

      import scala.reflect.classTag
      import scala.reflect.runtime.universe.{typeTag, weakTypeTag}

      val clsTag = classTag[String]
      val typTag = typeTag[List[Int]]
      val weakTypTag = weakTypeTag[List[Int]]

      // Некоторые часто возникающие задачи

      // Как получить ClassTag, если есть класс (например, полученный через classOf[Int]):

      import scala.reflect.ClassTag

      val cls = classOf[Int]
      val clsTag2 = ClassTag[Int](cls)

      // Как получить класс, имея ClassTag:

      object Utils {
        def retrieveClass[T](a: List[T])(implicit tag: ClassTag[T]): Class[T] = {
          tag.runtimeClass.asInstanceOf[Class[T]]
        }
      }

      println {
        Utils.retrieveClass[Int](List(1, 2)) // вернет Class[Int]
      }

      // Как получить класс, имея TypeTag:

      import scala.reflect.runtime.universe.TypeTag

      object Utils2 {
        def retrieveClass[T](a: List[T])(implicit tag: TypeTag[T]): Class[T] = {
          tag.mirror.runtimeClass(tag.tpe).asInstanceOf[Class[T]]
        }
      }

      println {
        Utils2.retrieveClass[Int](List(1, 2)) // вернет Class[Int]
      }

      // "mirror" - это провайдер информации.
      // Вся информация, предоставленная рефлексией, доступна через "mirror".

      // Mirrors are a central part of Scala Reflection.
      // All information provided by reflection is made accessible through Mirrors.
      // Depending on the type of information to be obtained, or the reflective action to be taken,
      // different flavors of mirrors must be used.
      // "Classloader" mirrors can be used to obtain representations of types and members.
      // From a classloader Mirror, it's possible to obtain more specialized "invoker" Mirrors
      // (the most commonly-used mirrors), which implement reflective invocations,
      // such as method/constructor calls and field accesses.

      // Как получить TypeTag, если есть класс:

      import scala.reflect.api
      import scala.reflect.runtime.universe._

      object Utils3 {
        def retrieveTag[T](cls: Class[T]): TypeTag[T] = {
          val mirror = runtimeMirror(cls.getClassLoader)
          val typ = mirror.classSymbol(cls).selfType
          TypeTag[T](mirror, new api.TypeCreator {
            def apply[U <: api.Universe with Singleton](m: api.Mirror[U]): U # Type =
              if (m eq mirror) typ.asInstanceOf[U # Type]
              else throw new IllegalArgumentException(s"Type tag defined in $mirror cannot be " +
                s"migrated to other mirrors.")
          })
        }
      }


      println {
        Utils3.retrieveTag[Int](classOf[Int]) // вернет TypeTag[Int]
      }

      // Как сравнить типы, имея TypeTag типа:

      import scala.reflect.runtime.universe._

      object Utils4 {
        def retrieveMsg[T: TypeTag](a: List[T]): String = {
          typeOf[T] match {
            case t if t =:= typeOf[String] =>
              "List of strings"
            case t if t <:< typeOf[Foo] =>
              "List of `Foo` inheritor instances"
          }
        }
      }

      println {
        Utils4.retrieveMsg[String](List("s1", "s2")) // вернет "List of strings"
      }

      println {
        Utils4.retrieveMsg[Bar](List(new Bar, new Bar)) // вернет "List of `Foo` inheritor instances"
      }

      println("---------")
    }
  }

  part1.chapter1()
  part1.chapter2()
  part1.chapter3()

  part2.chapter1()
  part2.chapter2()
  part2.chapter3()
  part2.chapter4()

}

// Следует отметить, что, так как использование тегов типов подразумевает рефлексию,
// то это может значительно замедлить программу, поэтому использовать их нужно только там,
// где это действительно необходимо.