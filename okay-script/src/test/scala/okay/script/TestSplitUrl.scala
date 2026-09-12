package okay.script

import scala.compiletime.testing.typeChecks

/**
 * `Site.splitUrl` answers a NAMED pair (split-url-named, 2026-09-12).
 *
 * It is public API returning two `String`s, so a caller outside this
 * repository could take them the wrong way round and get the query
 * string as the path, compiling. The names make the right reading the
 * short one; this suite pins both the split itself and the refusal of
 * a wrong name.
 */
class TestSplitUrl extends munit.FunSuite {

  test("a url splits into path and query, by name") {
    val s = Site.splitUrl("/a/b?x=1&y=2")
    assertEquals(s.path, "/a/b")
    assertEquals(s.query, "x=1&y=2")
  }

  test("no '?' means an empty query, not an empty path") {
    val s = Site.splitUrl("/a/b")
    assertEquals(s.path, "/a/b")
    assertEquals(s.query, "")
  }

  test("pathOf reads the name rather than the position") {
    assertEquals(Site.pathOf("/a?b=1"), "/a")
  }

  test("a wrong name does not compile, paired with the right one") {
    assert(typeChecks("""val p: String = okay.script.Site.splitUrl("/a?b=1").query"""))
    assert(!typeChecks("""val p: String = okay.script.Site.splitUrl("/a?b=1").queryString"""))
  }
}
