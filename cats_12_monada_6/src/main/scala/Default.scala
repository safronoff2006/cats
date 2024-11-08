
//type class
case class Default[F](a: F)  {
  def get: F = a
}

object Default {
  //инстансы
  implicit val defaultInt: Default[Int] = Default[Int](0)
  implicit val defailtString: Default[String] = Default[String]("Default")
  implicit val defaultBoolean: Default[Boolean] = Default[Boolean](false)

  //суммонер
  def apply[A](implicit default: Default[A]): Default[A] = default

}

