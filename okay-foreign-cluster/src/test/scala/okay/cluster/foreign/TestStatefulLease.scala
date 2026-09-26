package okay.cluster.foreign

import java.util.concurrent.atomic.AtomicInteger
import okay.{Aggregator, Chunks}
import okay.cluster.{Flow, Flows, Scope}
import okay.given

/**
 * A stateful stage gives back what holds a partition's state on EVERY path
 * (foreign-one-pool). Found on feature/foreign-streams-holds: a step that
 * failed threw out of the partition's iterator with its interpreter still
 * leased, so each ordinary failure kept one worker of the pool for good,
 * and a pool of `workers` starved after that many. Default gate: a counting
 * `Streamer`, no interpreter.
 */
class TestStatefulLease extends munit.FunSuite:

  /** counts every state opened and every state given back (finish or
   * abandon); `failAt` makes that step refuse */
  final class Counting(failAt: Int) extends Streamer[Int, Int]:
    val name = "counting"
    type S = Int
    val opened = AtomicInteger()
    val givenBack = AtomicInteger()
    private var steps = 0
    def open(): Either[Batcher.Failed, Int] = { opened.incrementAndGet(): Unit; Right(0) }
    def step(s: Int, rows: Vector[Int]): Either[Batcher.Failed, Vector[Int]] =
      steps += 1
      if steps == failAt then Left(Batcher.Failed("ValueError", "step says no")) else Right(rows)
    def finish(s: Int): Either[Batcher.Failed, Vector[Int]] = { givenBack.incrementAndGet(): Unit; Right(Vector.empty) }
    def abandon(s: Int): Unit = givenBack.incrementAndGet(): Unit

  private def drain(c: Chunks[Int]): Vector[Int] =
    val out = Vector.newBuilder[Int]
    var rest = c
    var more = true
    while more do Chunks.pull(rest) match
      case Some((chunk, r)) => chunk.foreach(out += _); rest = r
      case None => more = false
    out.result()

  private def partition(st: Streamer[Int, Int], scope: Scope = Scope(), rows: Int = 10): Chunks[Int] =
    Stateful.stateful(Chunks.rechunk(Chunks.fromIterator((1 to rows).iterator))(2), st, scope)

  test("a partition that finishes gives its state back once") {
    val st = Counting(failAt = -1)
    assertEquals(drain(partition(st)), (1 to 10).toVector)
    assertEquals((st.opened.get, st.givenBack.get), (1, 1))
  }

  test("a partition whose step fails gives its state back all the same") {
    val st = Counting(failAt = 2)
    val refused = intercept[okay.cluster.Cluster.Refused](drain(partition(st)))
    assert(refused.getMessage.contains("step says no"), refused.getMessage)
    assertEquals((st.opened.get, st.givenBack.get), (1, 1), "the failed partition kept its worker")
  }

  test("a partition whose consumer stops early gives its state back when the partition ends (stateful-early-stop)") {
    val st = Counting(failAt = -1)
    val scope = Scope()
    // past one chunk of the stage's output (64 rows), so `take` really stops it
    assertEquals(drain(Chunks.take(partition(st, scope, rows = 1000))(3)), Vector(1, 2, 3))
    assertEquals(st.givenBack.get, 0, "nothing downstream said it stopped: only the partition's end can")
    scope.close()
    assertEquals((st.opened.get, st.givenBack.get), (1, 1), "the stopped partition kept its worker")
    scope.close()
    assertEquals(st.givenBack.get, 1, "a scope closes once")
  }

  test("through the engine: a take after a stateful stage, and the state is given back once") {
    val st = Counting(failAt = -1)
    // past one chunk of the stage's output (64 rows), so `take` really
    // stops the partition before its end
    val staged = Stateful.through(Flow.slices(1 to 1000, 1), st, 2)
    val taken = Flow.Local(staged, "take", (c: Chunks[Int]) => Chunks.take(c)(3))
    assertEquals(Flows.fold(taken, Aggregator.count[Int]).runWith, 3L)
    assertEquals((st.opened.get, st.givenBack.get), (1, 1))
  }

  test("through the engine, read to its end: finish gives the state back, and the scope does not again") {
    val st = Counting(failAt = -1)
    assertEquals(Flows.fold(Stateful.through(Flow.slices(1 to 10, 1), st, 2), Aggregator.count[Int]).runWith, 10L)
    assertEquals((st.opened.get, st.givenBack.get), (1, 1))
  }
