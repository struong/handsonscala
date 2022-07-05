package chapter06

class ImmutableTrieSpec extends munit.FunSuite {
  test("Add/Contains") {
    val t = new ImmutableTrie(Seq("mango", "mandarin", "map", "man"))

    assert(t.contains("mango") == true)
    assert(t.contains("mang") == false)
    assert(t.contains("man") == true)
    assert(t.contains("mandarin") == true)
    assert(t.contains("mandarine") == false)
  }
}
