package chapter06

import scala.annotation.tailrec

object BinarySearch {

  def search(target: Int, values: List[Int]): Option[Int] = {

    @tailrec
    def go(left: Int, right: Int): Option[Int] = {
      if (left <= right) {
        val m = math.floor((left + right) / 2).toInt
        if (values(m) < target)
          go(m + 1, right)
        else if (values(m) > target)
          go(left, m - 1)
        else Some(values(m))
      } else {
        None
      }
    }

    if (values.isEmpty) None
    else go(0, values.size)
  }
}
