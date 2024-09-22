
import cats._
//import cats.implicits._
import cats.instances.all._
import cats.syntax.all._


object Main extends App {
  println("Изучаем Cats!".toUpperCase)

  object custom_monoid_implementation {

    implicit val booleanAndMonoid: Monoid[Boolean] =
      new Monoid[Boolean] {
        override def empty = true

        override def combine(a: Boolean, b: Boolean): Boolean = a && b
      }

    implicit val booleanOrMonoid: Monoid[Boolean] =
      new Monoid[Boolean] {
        override def empty: Boolean = false

        override def combine(a: Boolean, b: Boolean): Boolean = a || b
      }

    implicit val booleanXnorMonoid: Monoid[Boolean] =
      new Monoid[Boolean] {
        override def empty: Boolean = true

        override def combine(a: Boolean, b: Boolean): Boolean =
          (!a || b) && (a || !b)
      }

    def setUniuonMonoid[A]: Monoid[Set[A]] =
      new Monoid[Set[A]] { self =>
        override def empty: Set[A] = Set.empty[A]

        override def combine(a: Set[A], b: Set[A]): Set[A] = a union b


      }

    def setDiffMonoid[A]: Monoid[Set[A]] =
      new Monoid[Set[A]] {
        override def empty: Set[A] = Set.empty[A]

        override def combine(a: Set[A], b: Set[A]): Set[A] = (a diff b) union (b diff a)
      }

    implicit def setIntersectSemi[A]: Semigroup[Set[A]] = {
      (a: Set[A], b: Set[A]) => a intersect b
    }


  }

  def exercise: Unit = {

    import custom_monoid_implementation._

    println(s"And combine true true =   ${booleanAndMonoid.combine(true, true)}")
    println(s"And combine false true =  ${booleanAndMonoid.combine(false, true)}")
    println(s"And combine false true =  ${booleanAndMonoid.combine(false, false)}")

    println(s"Or combine true true =   ${booleanOrMonoid.combine(true, true)}")
    println(s"Or combine false true =   ${booleanOrMonoid.combine(false, true)}")
    println(s"Or combine false false =  ${booleanOrMonoid.combine(false, false)}")

    ///// менее абстрактно
    val strSetMonoid = Monoid[Set[String]](setUniuonMonoid)
    println(strSetMonoid.combine(Set("Коля", "Вася"), Set("Валера")).show)

    final case class Man(name: String, age: Int) {
      def show = s"(${name.show},${age.show})"
    }

    val manSetUnionMonoid: Monoid[Set[Man]] = Monoid[Set[Man]](setUniuonMonoid)

    println(manSetUnionMonoid.combine(Set(Man("Коля", 40), Man("Вася", 40)), Set(Man("Игорь", 20), Man("Вася", 40))))

    val manSetDiffMonoid: Monoid[Set[Man]] = Monoid[Set[Man]](setDiffMonoid)
    println(manSetDiffMonoid.combine(Set(Man("Коля", 40), Man("Вася", 40)), Set(Man("Игорь", 20), Man("Вася", 40))))

    ///// более абстрактно
    def combineSetsM[A](monoid: Monoid[Set[A]], a: Set[A], b: Set[A]): Set[A] = monoid.combine(a, b)

    println(combineSetsM(setUniuonMonoid, Set(Man("Коля", 40), Man("Вася", 40)), Set(Man("Игорь", 20), Man("Вася", 40))))
    println(combineSetsM(setDiffMonoid, Set(Man("Коля", 40), Man("Вася", 40)), Set(Man("Игорь", 20), Man("Вася", 40))))

    //// менее абстрактно
    val manSetInersectSemi = Semigroup[Set[Man]]
    println(manSetInersectSemi.combine(Set(Man("Коля", 40), Man("Вася", 40)), Set(Man("Игорь", 20), Man("Вася", 40))))

    /// более абстрактно

    def combineSetsS1[A](a: Set[A], b: Set[A])(implicit semi: Semigroup[Set[A]]): Set[A] = semi.combine(a, b)

    def combineSetsS2[A](a: Set[A], b: Set[A]): Set[A] = implicitly[Semigroup[Set[A]]].combine(a, b)


    println(manSetInersectSemi.combine(Set(Man("Коля", 40), Man("Вася", 40)), Set(Man("Игорь", 20), Man("Вася", 40))))
    println(combineSetsS1(Set(Man("Коля", 40), Man("Вася", 40)), Set(Man("Игорь", 20), Man("Вася", 40))))
    println(combineSetsS1(Set(1, 2, 4, 8), Set(4, 5, 6, 7, 8)))
    println(combineSetsS2(Set(Man("Коля", 40), Man("Вася", 40)), Set(Man("Игорь", 20), Man("Вася", 40))))
    println(combineSetsS2(Set(1, 2, 4, 8), Set(4, 5, 6, 7, 8)).show)

    println((Set(1,2,3) |+| Set(3,4,5)).show)
  }

  exercise

}
