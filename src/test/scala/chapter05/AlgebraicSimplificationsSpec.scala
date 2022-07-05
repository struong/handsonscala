package chapter05

class AlgebraicSimplificationsSpec extends munit.FunSuite {
  val example = BinOp(Literal(1), "+", Literal(1))

  test("stringify (1 + 1)") {
    assertEquals(AlgebraicSimplifications.stringify(example), "(1 + 1)")
  }

  test("simplify (1 + 1") {
    val example = BinOp(Literal(1), "+", Literal(1))

    val actual = AlgebraicSimplifications.stringify(
      AlgebraicSimplifications.simplify(example)
    )

    assertEquals(actual, "2")
  }

  test("simplify ((1 + 1) * x") {
    val example = BinOp(BinOp(Literal(1), "+", Literal(1)), "*", Variable("x"))

    val actual = AlgebraicSimplifications.stringify(
      AlgebraicSimplifications.simplify(example)
    )

    assertEquals(actual, "(2 * x)")
  }

  test("simplify ((2 - 1) * x") {
    val example = BinOp(
      BinOp(Literal(2), "-", Literal(1)),
      "*",
      Variable("x")
    )

    val actual = AlgebraicSimplifications.stringify(
      AlgebraicSimplifications.simplify(example)
    )

    assertEquals(actual, "x")
  }

  test("simplify (((1 + 1) * y) + ((1 - 1) * x)))") {
    val example = BinOp(
      BinOp(BinOp(Literal(1), "+", (Literal(1))), "*", Variable("y")),
      "+",
      BinOp(BinOp(Literal(1), "-", (Literal(1))), "*", Variable("x"))
    )

    val actual = AlgebraicSimplifications.stringify(
      AlgebraicSimplifications.simplify(example)
    )

    assertEquals(actual, "(2 * y)")
  }
}
