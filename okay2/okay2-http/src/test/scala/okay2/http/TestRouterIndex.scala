package okay2.http

import okay2.{!, pure}
import okay2.async.Async
import Drive.run

/** the trie answers WHO IS ASKED; the entry decides (okay-http's
 * TestRouterIndex): agreement with the first-match scan on generated
 * tables, definedness and which entry answers */
class TestRouterIndex extends munit.FunSuite {

  private val words = Vector("a", "b", "c", "items", "users", "x")
  private val methods = Method.values

  private def answering(status: Int): Response ! Async = pure[Async, Response](Response(status, Nil, Http.one(Array.empty[Byte])))

  /** a random route of 0–3 segments; the handler answers its own index as a status */
  private def table(rnd: scala.util.Random): Router = {
    val n = 1 + rnd.nextInt(40)
    (0 until n).foldLeft(Router.empty) { (r, i) =>
      val m = methods(rnd.nextInt(methods.length))
      def w = words(rnd.nextInt(words.length))
      val status = 200 + i
      rnd.nextInt(4) match {
        case 0 => r.on(m, Route.root)(_ => answering(status))
        case 1 => if (rnd.nextBoolean()) r.on(m, Route / w)(_ => answering(status)) else r.on(m, Route[Int]("p"))(_ => answering(status))
        case 2 => if (rnd.nextBoolean()) r.on(m, Route / w / Route[Int]("p"))(_ => answering(status)) else r.on(m, Route / w / w)(_ => answering(status))
        case _ => r.on(m, Route / w / Route[Int]("p") / w)(_ => answering(status))
      }
    }
  }

  private def request(rnd: scala.util.Random): Request = {
    val m = methods(rnd.nextInt(methods.length))
    def w = words(rnd.nextInt(words.length))
    val segs = rnd.nextInt(5) match {
      case 0 => ""
      case 1 => "/" + w
      case 2 => "/" + w + "/" + (if (rnd.nextBoolean()) rnd.nextInt(50).toString else w)
      case 3 => "/" + w + "/" + rnd.nextInt(50) + "/" + w
      case _ => "/%zz/bad"
    }
    Request(m, if (segs.isEmpty) "/" else segs)
  }

  private def scan(r: Router, req: Request): Option[Int] = r.entries.indexWhere(_.matches(req)) match {
    case -1 => None
    case i => Some(i)
  }

  test("the index agrees with the scan on 300 generated tables x 50 requests") {
    val rnd = new scala.util.Random(20260923)
    for (_ <- 1 to 300) {
      val r = table(rnd)
      val pf = r.routes
      for (_ <- 1 to 50) {
        val req = request(rnd)
        val expected = scan(r, req)
        assertEquals(pf.isDefinedAt(req), expected.isDefined, s"${req.method} ${req.url} on ${r.describe}")
        val got = run(pf.applyOrElse(req, (_: Request) => answering(0))).status
        assertEquals(got, expected.map(_ + 200).getOrElse(0), s"${req.method} ${req.url} on ${r.describe}")
      }
    }
  }

  test("candidates are a superset of the matching entries, sorted in declaration order") {
    val rnd = new scala.util.Random(20260924)
    for (_ <- 1 to 200) {
      val r = table(rnd)
      val ix = new Router.Index(r.entries)
      for (_ <- 1 to 30) {
        val req = request(rnd)
        val cs = ix.candidates(req)
        assertEquals(cs, cs.sorted)
        for ((e, i) <- r.entries.zipWithIndex if e.matches(req))
          assert(cs.contains(i), s"entry $i (${e.method} ${e.path}) matches ${req.method} ${req.url} but is not a candidate: $cs")
      }
    }
  }

  test("++ keeps the concatenated order; a literal that looks like a param is a superset, still right") {
    val a = Router.empty.on(Method.Get, Route / "x" / Route[Int]("id"))(_ => answering(201))
    val b = Router.empty.on(Method.Get, Route / "x" / "7")(_ => answering(202))
    def status(r: Router, url: String): Int = run(r.routes(Request.get(url))).status
    assertEquals(status(a ++ b, "/x/7"), 201, "first declared wins")
    assertEquals(status(b ++ a, "/x/7"), 202)
    val odd = Router.empty.on(Method.Get, Route / "{weird}")(_ => answering(203))
    assertEquals(status(odd, "/{weird}"), 203)
    assert(!odd.routes.isDefinedAt(Request.get("/other")), "the entry still decides")
  }
}
