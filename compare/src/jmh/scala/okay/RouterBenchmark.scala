package okay

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit
import okay.http.*

/**
 * What `Router.routes` costs per request at 3, 30 and 300 routes
 * (specs/router-trie.md): a request hitting the LAST route, and a
 * miss. On master the table is a first-match scan; on router-trie an
 * index names the candidates. The handler's program is built, not
 * run — dispatch is what differs.
 */
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class RouterBenchmark {

  private def table(n: Int): Router =
    (0 until n).foldLeft(Router.empty) { (r, i) =>
      r.on(Method.Get, Route / s"svc$i" / "items" / Route[Int]("id"))(_ => pure(Response(200, Nil, Http.one(Array.empty[Byte]))))
    }

  val pf3 = table(3).routes
  val pf30 = table(30).routes
  val pf300 = table(300).routes

  val last3 = Request.get("/svc2/items/7")
  val last30 = Request.get("/svc29/items/7")
  val last300 = Request.get("/svc299/items/7")
  val miss = Request.get("/nope/items/7")
  val fallback: Request => Response ! Async = _ => pure(Response(404, Nil, Http.one(Array.empty[Byte])))

  @Benchmark def hitLast3: Response ! Async = pf3.applyOrElse(last3, fallback)
  @Benchmark def hitLast30: Response ! Async = pf30.applyOrElse(last30, fallback)
  @Benchmark def hitLast300: Response ! Async = pf300.applyOrElse(last300, fallback)
  @Benchmark def miss300: Response ! Async = pf300.applyOrElse(miss, fallback)
  @Benchmark def defined300: Boolean = pf300.isDefinedAt(last300)
}
