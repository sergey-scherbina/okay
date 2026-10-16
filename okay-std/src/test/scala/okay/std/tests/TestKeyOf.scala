package okay.std.tests

import okay.*
import okay.std.*

/** the keys of effects, found at compile time: one a constructor class, shared */
class TestKeyOf extends munit.FunSuite {
  test("one key a constructor class, the same for any arguments; a different class a different key") {
    assert(summon[KeyOf[Writer % Int]].ref eq summon[KeyOf[Writer % String]].ref)
    assert(summon[KeyOf[Writer % Int]].ref eq classOf[Writer[?, ?]])
    assert(summon[KeyOf[State % Int]].ref ne summon[KeyOf[Writer % Int]].ref)
    assert(summon[KeyOf[Maybe]].ref eq classOf[Maybe[?]])
  }
  test("an effect with no class of its own keyed by its name") {
    val k = summon[KeyOf[Produce]].ref
    assert(k.isInstanceOf[String] && (k eq summon[KeyOf[Produce]].ref), k)
  }
  test("the key of an abstract effect is not made up") {
    assert(compileErrors("def k[E[_]] = summon[okay.KeyOf[E]]").contains("no key for the effect"))
  }
}
