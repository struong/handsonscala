package chapter05

import chapter05.Deserialise.splitExpressions

trait StrParser[T] {
  def parse(s: String): T
}

object StrParser {
  given ParseInt: StrParser[Int] with
    def parse(s: String): Int = s.toInt

  given ParseBoolean: StrParser[Boolean] with
    def parse(s: String): Boolean = s.toBoolean

  given ParseDouble: StrParser[Double] with
    def parse(s: String): Double = s.toDouble

  given parseSeq[T](using p: StrParser[T]): StrParser[Seq[T]] with
    def parse(s: String) = splitExpressions(s).map(p.parse)

  given parseTuple[T, V](using p1: StrParser[T], p2: StrParser[V]): StrParser[(T, V)] with
    def parse(s: String) = {
      val Seq(left, right) = splitExpressions(s)
      (p1.parse(left), p2.parse(right))
    }
}

object Deserialise {

  def splitExpressions(s: String): Seq[String] = {
    val indices = collection.mutable.ArrayDeque.empty[Int]
    var openBrackets = 0
    for(i <- Range(1, s.length - 1)){
      s(i) match {
        case '[' => openBrackets += 1
        case ']' => openBrackets -= 1
        case ',' =>
          if (openBrackets == 0) indices += i
        case _ => // do nothing
      }
    }
    val allIndices = Seq(0) ++ indices ++ Seq(s.length - 1)
    for(i <- Range(1, allIndices.length).toList)
      yield s.substring(allIndices(i - 1) + 1, allIndices(i))
  }

  def parseFromString[T: StrParser](s: String) = {
    import StrParser._
    implicitly[StrParser[T]].parse(s)
  }
}
