package chapter05

trait StrSerialiser[T] {
  def serialise(t: T): String
}

object StrSerialiser {
  given WriteBoolean: StrSerialiser[Boolean] with
    override def serialise(t: Boolean): String = t.toString

  given WriteInt: StrSerialiser[Int] with
    override def serialise(t: Int): String = t.toString

  given WriteDouble: StrSerialiser[Double] with
    override def serialise(t: Double): String = t.toString

  given WriteSeq[T](using s: StrSerialiser[T]): StrSerialiser[Seq[T]] with
    override def serialise(t: Seq[T]): String = t.map(s.serialise).mkString("[", ",", "]")

  given WriteTuple[T, V](using s1: StrSerialiser[T], s2: StrSerialiser[V]): StrSerialiser[(T, V)] with
    override def serialise(t: (T, V)): String = "[" ++ s1.serialise(t._1) ++ "," ++ s2.serialise(t._2) ++ "]"
}

object Serialise {
  def writeToString[T: StrSerialiser](t: T): String = {
    import StrSerialiser._
    implicitly[StrSerialiser[T]].serialise(t)
  }
}
