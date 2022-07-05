package chapter05

class DeserialiseSpec extends munit.FunSuite {
  test("1 layer of nesting") {
    assertEquals(
      Deserialise.parseFromString[Seq[Boolean]]("[true,false,true]"),
      Seq(true, false, true)
    )
  }
  test(" 3 layers of nesting") {
    assertEquals(
      Deserialise.parseFromString[Seq[(Seq[Int], Seq[Boolean])]](
        "[[[1],[true]],[[2,3],[false,true]],[[4,5,6],[false,true,false]]]"
      ),
      Seq(
        (Seq(1), Seq(true)),
        (Seq(2, 3), Seq(false, true)),
        (Seq(4, 5, 6), Seq(false, true, false))
      )
    )
  }

  test("4 layers of nesting") {
    assertEquals(
      Deserialise.parseFromString[Seq[(Seq[Int], Seq[(Boolean, Double)])]](
        "[[[1],[[true,0.5]]],[[2,3],[[false,1.5],[true,2.5]]]]"
      ),
      Seq(
        (Seq(1), Seq((true, 0.5))),
        (Seq(2, 3), Seq((false, 1.5), (true, 2.5)))
      )
    )
  }
}
