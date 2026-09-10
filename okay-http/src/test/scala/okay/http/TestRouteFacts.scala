package okay.http

import okay.*

/**
 * WHY A FACT AND NOT A CAPABILITY, on the case that motivates it
 * (di-facts-examples).
 *
 * Several feature modules each own part of a service's surface, and
 * the server must serve ALL of it. Installing does not do that:
 * `and` shadows, so two modules installing routes leave the second's
 * and lose the first's — which is right for a capability (a test
 * double must be able to replace one) and wrong for a contribution.
 *
 * A FACT accumulates instead, by whatever rule its kind states. For
 * routes that rule is `orElse`, so the kind is one declaration and
 * the merge is the one every server in this stack already uses.
 */
class TestRouteFacts extends munit.FunSuite {

  type Routes = Request |=> Response ! Async

  /** the kind: routes accumulate, first match wins — `orElse` */
  given Monoid[Routes] = Monoid.of(PartialFunction.empty[Request, Response ! Async])(_ orElse _)
  object Surface extends Fact[Routes]

  final class Board { def items: Vector[String] = Vector("one", "two") }
  final class Admin { def token: String = "t0ken" }

  private def text(s: String): Response ! Async = pure(Response(200, Nil, Http.one(s.getBytes("UTF-8"))))

  /**
   * A FEATURE IS ONE MODULE: its capability and the routes that use
   * it, together (fact-declaring). `declaring` computes the fact
   * inside this module's own installer, so `wire[Board]` here is the
   * `Board` this very module installs.
   */
  val boardFeature: Module[[X] =>> Board ?=> X] =
    Module.value[Board](Board()).declaring(Surface) {
      case r if r.url == "/board" => text(wire[Board].items.mkString(","))
    }

  /** a feature may still read what came BEFORE it, like any module */
  def adminFeature: Board ?=> Module[[X] =>> Admin ?=> X] =
    Module.value[Admin](Admin()).declaring(Surface) {
      case r if r.url == "/admin" => text(wire[Admin].token)
      case r if r.url == "/admin/count" => text(wire[Board].items.size.toString)
    }

  /**
   * And a contributor that owns NO capability — a feature written
   * over somebody else's — is `Module.contributing`, which installs
   * nothing. Both spellings are curried, so the block takes its type
   * from `Fact[V]` and needs no ascription.
   */
  def health: Board ?=> Module[[X] =>> X] =
    Module.contributing(Surface) {
      case r if r.url == "/healthz" => text(if wire[Board].items.nonEmpty then "ok" else "empty")
    }

  // `runAsync`, not `run(...).runWith`: this file is compiled for JS
  // too, where the blocking API does not exist at compile time (that
  // is the platform law, not an accident) — so the assertions come
  // back as a Future and munit awaits it
  private def body(res: Response ! Async): scala.concurrent.Future[String] =
    Async.runAsync(res.flatMap(Http.text))

  test("a feature's own capability, one it reads, and a contributor that has none — all served") {
    val app = (boardFeature and adminFeature and health).installing(Surface)
    val answers = Resource.scoped(app {
      val routes = wire[Routes]
      Vector("/board", "/admin", "/admin/count", "/healthz").map(u => body(routes(Request.get(u))))
    })
    import scala.concurrent.ExecutionContext.Implicits.global
    scala.concurrent.Future.sequence(answers).map(got =>
      assertEquals(got, Vector("one,two", "t0ken", "2", "ok")))
  }

  test("the same shape as an INSTALL keeps only the last — which is why this is a fact") {
    // both modules install `Routes` instead of contributing it
    val a = Module.value[Routes]({ case r if r.url == "/board" => text("board") }: Routes)
    val b: Routes ?=> Module[[X] =>> Routes ?=> X] =
      Module.value[Routes]({ case r if r.url == "/admin" => text("admin") })
    val onlyLast = Resource.scoped((a and b) { wire[Routes].isDefinedAt(Request.get("/board")) })
    assertEquals(onlyLast, false)      // the first module's route is gone
  }

  test("a module that contributes nothing costs nothing, and the empty surface is the monoid's") {
    val plain = Module.value[Board](Board())
    assertEquals(Resource.scoped(plain.installing(Surface) {
      wire[Routes].isDefinedAt(Request.get("/board"))
    }), false)
  }
}
