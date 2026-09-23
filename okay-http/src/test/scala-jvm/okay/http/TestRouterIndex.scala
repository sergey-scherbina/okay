package okay.http

import okay.*
import scala.concurrent.Await
import scala.concurrent.duration.*

/**
 * specs/router-trie.md: the index answers WHO IS ASKED; the entry
 * decides. Agreement with the first-match scan on generated tables.
 */
class TestRouterIndex extends munit.FunSuite:

  private val words = Vector("a", "b", "c", "items", "users", "x")
  private val methods = Method.values.toVector

  /** a random route of 0–3 segments, literals and params mixed; the handler answers its own index as a status */
  private def table(rnd: scala.util.Random): Router =
    val n = 1 + rnd.nextInt(40)
    (0 until n).foldLeft(Router.empty) { (r, i) =>
      val m = methods(rnd.nextInt(methods.length))
      val depth = rnd.nextInt(4)
      val status = 200 + i
      depth match
        case 0 => r.on(m, Route.root)(_ => pure(Response(status, Nil, Http.one(Array.empty[Byte]))))
        case 1 =>
          if rnd.nextBoolean() then r.on(m, Route / words(rnd.nextInt(words.length)))(_ => pure(Response(status, Nil, Http.one(Array.empty[Byte]))))
          else r.on(m, Route[Int]("p"))(_ => pure(Response(status, Nil, Http.one(Array.empty[Byte]))))
        case 2 =>
          if rnd.nextBoolean() then r.on(m, Route / words(rnd.nextInt(words.length)) / Route[Int]("p"))(_ => pure(Response(status, Nil, Http.one(Array.empty[Byte]))))
          else r.on(m, Route / words(rnd.nextInt(words.length)) / words(rnd.nextInt(words.length)))(_ => pure(Response(status, Nil, Http.one(Array.empty[Byte]))))
        case _ =>
          r.on(m, Route / words(rnd.nextInt(words.length)) / Route[Int]("p") / words(rnd.nextInt(words.length)))(_ => pure(Response(status, Nil, Http.one(Array.empty[Byte]))))
    }

  private def request(rnd: scala.util.Random): Request =
    val m = methods(rnd.nextInt(methods.length))
    val segs = rnd.nextInt(5) match
      case 0 => ""
      case 1 => "/" + words(rnd.nextInt(words.length))
      case 2 => "/" + words(rnd.nextInt(words.length)) + "/" + (if rnd.nextBoolean() then rnd.nextInt(50).toString else words(rnd.nextInt(words.length)))
      case 3 => "/" + words(rnd.nextInt(words.length)) + "/" + rnd.nextInt(50) + "/" + words(rnd.nextInt(words.length))
      case _ => "/%zz/bad"                      // a malformed escape: a miss everywhere
    Request(m, if segs.isEmpty then "/" else segs)

  /** the reference: first-match over the entries, as the scan did */
  private def scan(r: Router, req: Request): Option[Int] =
    r.entries.indexWhere(_.matches(req)) match
      case -1 => None
      case i => Some(i)

  test("the index agrees with the scan on 300 generated tables × 50 requests: definedness and WHICH entry answers") {
    val rnd = new scala.util.Random(20260923)
    for _ <- 1 to 300 do
      val r = table(rnd)
      val pf = r.routes
      for _ <- 1 to 50 do
        val req = request(rnd)
        val expected = scan(r, req)
        assertEquals(pf.isDefinedAt(req), expected.isDefined, s"${req.method} ${req.url} on ${r.describe}")
        val got = pf.applyOrElse(req, _ => pure(Response(0, Nil, Http.one(Array.empty[Byte]))))
        val status = Await.result(Async.runAsync(got), 5.seconds).status
        assertEquals(status, expected.map(_ + 200).getOrElse(0), s"${req.method} ${req.url} on ${r.describe}")
  }

  test("candidates ⊇ the matching entries, and are sorted in declaration order") {
    val rnd = new scala.util.Random(20260924)
    for _ <- 1 to 200 do
      val r = table(rnd)
      val ix = Router.Index(r.entries)
      for _ <- 1 to 30 do
        val req = request(rnd)
        val cs = ix.candidates(req)
        assertEquals(cs, cs.sorted)
        for (e, i) <- r.entries.zipWithIndex if e.matches(req) do
          assert(cs.contains(i), s"entry $i (${e.method} ${e.path}) matches ${req.method} ${req.url} but is not a candidate: $cs")
  }

  test("++ keeps the concatenated order; a literal that looks like a param is a superset, still right") {
    val a = Router.empty.on(Method.Get, Route / "x" / Route[Int]("id"))(_ => pure(Response(201, Nil, Http.one(Array.empty[Byte]))))
    val b = Router.empty.on(Method.Get, Route / "x" / "7")(_ => pure(Response(202, Nil, Http.one(Array.empty[Byte]))))
    def status(r: Router, url: String): Int =
      Await.result(Async.runAsync(r.routes(Request.get(url))), 5.seconds).status
    assertEquals(status(a ++ b, "/x/7"), 201, "first declared wins")
    assertEquals(status(b ++ a, "/x/7"), 202)
    val odd = Router.empty.on(Method.Get, Route / "{weird}")(_ => pure(Response(203, Nil, Http.one(Array.empty[Byte]))))
    assertEquals(status(odd, "/{weird}"), 203)
    assert(!odd.routes.isDefinedAt(Request.get("/other")), "the entry still decides")
  }
