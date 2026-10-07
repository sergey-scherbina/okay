package okay.freer



import okay.std.*
/**
 * `PState.Threaded.zoomWith` (specs/cont-js-depth.md stage 3a): a
 * typestate program over a part, run over the whole, as ONE operation of
 * the tree — the type-changing zoom of the shift road without its nested
 * run. On every platform.
 */
class TestThreadedZoom extends munit.FunSuite:
  import PState.Threaded.{get, put, zoomWith}

  final case class Box[A](item: A, tag: String)

  test("the part goes String -> Int, so the whole goes Box[String] -> Box[Int]; the rest rides along") {
    val parse: PState.Threaded[Int, Int, String] =
      for
        s <- get[String]
        _ <- put[String, Int](s.length)
      yield s.length
    val prog = zoomWith[Box[String], Box[Int], String, Int, Int](_.item, (b, i) => Box(i, b.tag))(parse)
    assertEquals(PState.Threaded.run(prog)(Box("hello", "t")), (Box(5, "t"), 5))
  }

  test("a zoom inside a zoom: two lenses, two levels, both put back in order") {
    final case class Outer(box: Box[Int], n: Int)
    val inc: PState.Threaded[Int, Int, Int] = get[Int].flatMap(i => put[Int, Int](i + 1).map(_ => i))
    val inner = zoomWith[Box[Int], Box[Int], Int, Int, Int](_.item, (b, i) => b.copy(item = i))(inc)
    val prog = for
      a <- zoomWith[Outer, Outer, Box[Int], Box[Int], Int](_.box, (o, b) => o.copy(box = b))(inner)
      o <- get[Outer]
    yield (a, o.box.item, o.n)
    assertEquals(PState.Threaded.run(prog)(Outer(Box(41, "t"), 7))._2, (41, 42, 7))
  }

  test("a misused new state type does not compile") {
    val e = compileErrors("""
      final case class Box[A](item: A, tag: String)
      val parse: PState.Threaded[Int, Int, String] =
        PState.Threaded.get[String].flatMap(s => PState.Threaded.put[String, Int](s.length).map(_ => s.length))
      val bad: PState.Threaded[Int, Box[String], Box[String]] =
        PState.Threaded.zoomWith[Box[String], Box[Int], String, Int, Int](_.item, (b, i) => Box(i, b.tag))(parse)
    """)
    assert(e.nonEmpty, "a zoom leaving Box[Int] typed as Box[String]")
  }

  /** a million zooms, each INSIDE the last: one counter, a million levels down */
  // built from the inside out by a LOOP: an argument is evaluated
  // eagerly, so writing it as `zoomWith(…)(nested(n - 1))` would spend a
  // host frame per level on BUILDING the tree, before anything runs
  def nested(n: Int): PState.Threaded[Int, Int, Int] =
    (1 to n).foldLeft(get[Int].flatMap(i => put[Int, Int](i + 1).map(_ => i)))((p, _) =>
      zoomWith[Int, Int, Int, Int, Int](identity, (_, a) => a)(p))

  test("a million nested zooms, and a million get/put steps") {
    assertEquals(PState.Threaded.run(nested(1000000))(0), (1, 0))
    def steps(n: Int): PState.Threaded[Int, Int, Int] =
      if n == 0 then get[Int] else get[Int].flatMap(i => put[Int, Int](i + 1).flatMap(_ => steps(n - 1)))
    assertEquals(PState.Threaded.run(steps(1000000))(0), (1000000, 1000000))
  }
