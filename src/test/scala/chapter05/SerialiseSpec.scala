package chapter05

import Serialise._
import StrSerialiser._

class SerialiseSpec extends munit.FunSuite {
  test("serialise bools") {
    assertEquals(
      writeToString[Seq[Boolean]](Seq(true, false, true)),
      "[true,false,true]"
    )
  }

  test("types can be inferred") {
    assertEquals(
      writeToString(Seq(true, false, true)),
      "[true,false,true]"
    )
  }

  test("serialise ints") {
    assertEquals(
      writeToString(Seq(1, 2, 3)),
      "[1,2,3]"
    )
  }

  test("bunch of stuff") {
    assertEquals(
      writeToString[Seq[(Seq[Int], Seq[Boolean])]](
        Seq(
          (Seq(1), Seq(true)),
          (Seq(2, 3), Seq(false, true)),
          (Seq(4, 5, 6), Seq(false, true, false))
        )
      ),
      "[[[1],[true]],[[2,3],[false,true]],[[4,5,6],[false,true,false]]]"
    )
  }

  test("bunch of stuff p2") {
    assertEquals(
      writeToString(
        Seq(
          (Seq(1), Seq((true, 0.5))),
          (Seq(2, 3), Seq((false, 1.5), (true, 2.5)))
        )
      ),
      "[[[1],[[true,0.5]]],[[2,3],[[false,1.5],[true,2.5]]]]"
    )
  }
}
