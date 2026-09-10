package okay

/**
 * An instance per CONSUMER (di-prototype): what `New[A]` installs is
 * the ability to make an `A`, and the region a `fresh` runs in
 * releases it.
 */
class TestPrototype extends munit.FunSuite {

  final class Conn(val id: Int) { var closed = false }
  trait Log { def tag: String }

  test("fresh answers a new instance every time") {
    var made = 0
    val conns = prototype[Conn]({ made += 1; Conn(made) })
    val (a, b) = Resource.scoped(conns.use {
      fresh[Conn].flatMap(x => fresh[Conn].map(y => (x.id, y.id)))
    })
    assertEquals((a, b, made), (1, 2, 2))
  }

  /**
   * THE POINT OF ONE TYPE FOR BOTH (di-prototype). The consumer below
   * is written ONCE and used against both providers — the one with
   * nothing to close and the one that closes. A prototype whose pure
   * shape answered a bare `A` would have made this a rewrite the day
   * the provider started closing.
   */
  def twoIds: New[Conn] ?=> ((Int, Int) ! Resource) =
    fresh[Conn].flatMap(x => fresh[Conn].map(y => (x.id, y.id)))

  test("the call site does not change when the provider starts releasing") {
    var made = 0
    val plain = prototype[Conn]({ made += 1; Conn(made) })
    assertEquals(Resource.scoped(plain.use(twoIds)), (1, 2))

    made = 0
    var closed = List.empty[Int]
    val closing = prototype[Conn]({ made += 1; Conn(made) })(c => closed ::= c.id)
    assertEquals(Resource.scoped(closing.use(twoIds)), (1, 2))
    assertEquals(closed.sorted, List(1, 2))
  }

  test("the region that RUNS the fresh releases it — one per call, or one for the app") {
    var made = 0
    var closed = List.empty[Int]
    val conns = prototype[Conn]({ made += 1; Conn(made) })(c => closed ::= c.id)

    // a region per call: released at the call's end, before the next
    def handle(): New[Conn] ?=> (Int, Int) =
      (Resource.scoped(fresh[Conn].map(_.id)), closed.size)
    assertEquals(Resource.scoped(conns { (handle(), handle()) }), ((1, 1), (2, 2)))

    // the same fresh inside ONE region: nothing is released until it ends
    made = 0; closed = Nil
    val inOne = Resource.scoped(conns.use {
      fresh[Conn].flatMap(_ => fresh[Conn].map(_ => closed.size))
    })
    assertEquals(inOne, 0)
    assertEquals(closed.sorted, List(1, 2))
  }

  test("a prototype composes like any other module, and reads the ones before it") {
    val log = Module.value[Log](new Log { val tag = "t" })
    val conns: Log ?=> Module[[X] =>> New[Conn] ?=> X] =
      prototype[Conn](Conn(wire[Log].tag.length))
    assertEquals(Resource.scoped((log and conns).use(fresh[Conn].map(_.id))), 1)
    assertEquals((log and conns).plan, Vector("Log", "New[Conn]"))
  }

  test("Resource.scoped is the region as an expression") {
    var closed = false
    assertEquals(Resource.scoped(Resource.acquire(7)(_ => closed = true)), 7)
    assertEquals(closed, true)
  }
}
