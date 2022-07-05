package chapter06

class BinarySearchSpec extends munit.FunSuite {
  test("empty returns None") {
    assertEquals(BinarySearch.search(0, List.empty), None)
  }

  test("Single value returns self") {
    assertEquals(BinarySearch.search(1, List(1)), Some(1))
  }

  test("Not found returns None") {
    assertEquals(BinarySearch.search(0, List(1)), None)
  }

  test("Find stuff") {
    assertEquals(BinarySearch.search(2, List(1, 2, 3)), Some(2))
  }
}
