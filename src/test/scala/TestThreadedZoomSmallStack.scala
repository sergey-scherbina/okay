package okay

/** stage 3a on a 128 KB thread: the shift road's zoom held a host frame
 * per nesting; the threaded one holds none */
class TestThreadedZoomSmallStack extends munit.FunSuite:
  import PState.Threaded.{get, put, zoomWith}

  // built from the inside out by a LOOP: an argument is evaluated
  // eagerly, so writing it as `zoomWith(…)(nested(n - 1))` would spend a
  // host frame per level on BUILDING the tree, before anything runs
  def nested(n: Int): PState.Threaded[Int, Int, Int] =
    (1 to n).foldLeft(get[Int].flatMap(i => put[Int, Int](i + 1).map(_ => i)))((p, _) =>
      zoomWith[Int, Int, Int, Int, Int](identity, (_, a) => a)(p))

  test("a million nested zooms on 128 KB") {
    val p = nested(1000000)
    assertEquals(SmallStack.run(128)(PState.Threaded.run(p)(0)), (1, 0))
  }
