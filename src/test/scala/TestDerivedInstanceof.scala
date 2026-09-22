package okay

/**
 * typeablek-instanceof: a derived signature's test is a class of its
 * own with a constant-class `instanceof`, not `ByClass` reading a
 * field — and it still says exactly what `ByClass` said.
 */
class TestDerivedInstanceof extends munit.FunSuite:

  enum Users[+A] derives Effect:
    case Find(id: Int) extends Users[Option[String]]

  test("derives Effect no longer builds a ByClass (the field-read test)") {
    assert(!summon[Effect[Users]].isInstanceOf[Effect.ByClass[?]])
    assert(!summon[Effect[State % Int]].isInstanceOf[Effect.ByClass[?]], "a %-shaped signature too")
    assert(typeableK[Users](classOf[Users[?]]).isInstanceOf[Effect.ByClass[?]], "typeableK(cls) keeps ByClass: its class is a value")
  }

  test("the derived test agrees with ByClass on own, foreign and %-shaped operations") {
    val d = summon[Effect[Users]]
    val b = typeableK[Users](classOf[Users[?]])
    val ops: List[Any] = List(Users.Find(1), (State.Get(): State[Int, Int]), State.Set(2), Writer.Say("w"), (Reader.Ask(): Reader[Int, Int]), Throws("e"), 42, null)
    for op <- ops do assertEquals(d.test(op), b.test(op), s"$op")
    assert(summon[Effect[State % Int]].test(State.Set(1)))
    assert(summon[Effect[State % Int]].test((State.Get(): State[String, String])), "the erasure: State of any S")
    assert(!summon[Effect[State % Int]].test(Writer.Say(1)))
  }
