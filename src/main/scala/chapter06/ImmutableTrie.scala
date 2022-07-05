package chapter06

class ImmutableTrie(inputs: Seq[String]) {

  class Node(index: Int, inputs: Seq[String]) {
    val hasValue: Boolean = inputs.exists(_.length == index)
    val children: Map[Char, Node] = {
      val filteredInputs = inputs.filter(_.length > index)
      for((childChar, childInputs) <- filteredInputs.groupBy(_.charAt(index)))
      yield { 
        (childChar, new Node(index + 1, childInputs))
      }
    }
  }

  val root = new Node(0, inputs)

  def contains(s: String): Boolean = {
    var current = Option(root)
    for (c <- s if current.nonEmpty)
      current = current.get.children.get(c)
    current.exists(_.hasValue)
  }
}
