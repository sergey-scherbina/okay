package okay2

import Later.later

/**
 * Call-by-need for programs: `Once` is the effect, `!.once(p)` the
 * word, `Once.run` the handler whose state is the cells. A once'd
 * program runs at the first demand and answers from its cell after;
 * the tree holds no cell, so the same program run twice replays.
 */
class TestOnce extends munit.FunSuite {

  type W = Writer[String]
  type R = Once + W

  def logged[A](p: A ! R): (Seq[String], A) =
    !.run(Writer.run[String, A, W](Once.run[A, R](p)))

  test("!.once: three demands, one run, one value") {
    var hits = 0
    val q: Int ! R = !.once[Int, W](Free.delay(() => { hits += 1; pure[R, Int](hits * 10) }))
    val prog: Int ! R = q.flatMap(a => q.flatMap(b => q.map(c => a + b + c)))
    assertEquals(logged(prog), (Seq(), 30))
    assertEquals(hits, 1)
  }

  test("!.once: never demanded, never run — construction does no work") {
    var hits = 0
    val q: Int ! R = !.once[Int, W](Free.delay(() => { hits += 1; pure[R, Int](1) }))
    assertEquals(logged(pure[R, Int](7).map(_ + 1)), (Seq(), 8))
    assertEquals(hits, 0)
    val _ = q
  }

  test("!.once: what is in it is exactly what was passed — a bare p beside it runs every time") {
    val p: Int ! R = Writer.tell("p").at[R].map(_ => 1)
    val q: Int ! R = !.once[Int, W](p)
    val r: Int ! R = for { a <- q; b <- p; c <- q; d <- p } yield a + b + c + d
    assertEquals(logged(r), (Seq("p", "p", "p"), 4))
  }

  test("!.once: two calls are two handles") {
    val p: Int ! R = Writer.tell("p").at[R].map(_ => 1)
    val r: Int ! R = !.once[Int, W](p).flatMap(a => !.once[Int, W](p).map(b => a + b))
    assertEquals(logged(r), (Seq("p", "p"), 2))
  }

  test("!.once: a knot is a loud error, not a hang") {
    lazy val h: Int ! R = !.once[Int, W](h.map(_ + 1))
    val e = intercept[IllegalStateException](logged(h))
    assert(e.getMessage.contains("Once"), e.getMessage)
  }

  test("!.once: the same program run twice replays the same trace — the tree holds no cell") {
    var hits = 0
    val q: Int ! R = !.once[Int, W](Free.delay(() => { hits += 1; pure[R, Int](hits) }))
    val prog: Int ! R = q.flatMap(a => q.map(b => a + b))
    assertEquals(logged(prog), (Seq(), 2))
    assertEquals(logged(prog), (Seq(), 4))
    assertEquals(hits, 2)
  }

  test("Once.run forwards the rest of the row, and the cell survives a forwarded operation") {
    var hits = 0
    type R2 = Once + Later
    val q: Int ! R2 = !.once[Int, Later](later { hits += 1; 5 }.at[R2])
    val prog: Int ! R2 = q.flatMap(a => later(1).at[R2].flatMap(x => q.map(b => a + b + x)))
    assertEquals(Once.run[Int, R2](prog).runWith, 11)
    assertEquals(hits, 1)
  }

  // ---- multi-shot is handler order

  test("Once.run INSIDE the search: the cells backtrack, each branch runs its own once") {
    var hits = 0
    type Rw = Once + (Choose + W)
    val x: Int ! Rw = !.once[Int, Choose + W](Writer.tell("x").at[Rw].map(_ => { hits += 1; 10 }))
    val prog: Int ! Rw = choose(1, 2).at[Rw].flatMap(b => x.map(b + _))
    val handled: Seq[Int] ! W = runChoice[Int, Choose + W](Once.run[Int, Rw](prog))
    val (log, out) = !.run(Writer.run[String, Seq[Int], W](handled))
    assertEquals(out, Seq(11, 12))
    assertEquals(hits, 2)
    assertEquals(log, Seq("x", "x"))
  }

  test("Once.run OUTSIDE the search: one store, the second branch sees the first's value") {
    var hits = 0
    type Rw = Choose + (Once + W)
    val x: Int ! Rw = !.once[Int, W](Writer.tell("x").at[Once + W].map(_ => { hits += 1; 10 })).at[Rw]
    val prog: Int ! Rw = choose(1, 2).at[Rw].flatMap(b => x.map(b + _))
    val handled: Seq[Int] ! W = Once.run[Seq[Int], Once + W](runChoice[Int, Rw](prog))
    val (log, out) = !.run(Writer.run[String, Seq[Int], W](handled))
    assertEquals(out, Seq(11, 12))
    assertEquals(hits, 1)
    assertEquals(log, Seq("x"))
  }

  test("Once.run finds the effect anywhere in the row") {
    type R3 = Later + Once
    val q: Int ! R3 = Once.once[Int, Later](later(2).at[Once + Later]).at[R3]
    assertEquals(Once.run[Int, R3](q.flatMap(a => q.map(_ + a))).runWith, 4)
  }
}
