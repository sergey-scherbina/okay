package okay.caps

import scala.collection.mutable

class TestTxCapability extends okay.testkit.Munit.Diagnosed:
  override val munitTimeout = scala.concurrent.duration.Duration(180, "s")

  /** a database recording what it was asked */
  private final class Log extends Db:
    val said: mutable.ArrayBuffer[String] = mutable.ArrayBuffer.empty
    def begin(): Unit = said += "begin"
    def commit(): Unit = said += "commit"
    def rollback(): Unit = said += "rollback"
    def run(sql: String): Int = { said += sql; 1 }

  private val db = "object db extends Db { def begin() = (); def commit() = (); def rollback() = (); def run(s: String) = 1 }\n"
  private val outlives = "outlives its scope"

  private def refused(code: String): List[String] =
    val es = Compile.errors(db + code)
    note(s"$code\n  => $es")
    es

  test("a body using the transaction runs and commits; a failure rolls back") {
    val log = Log()
    assertEquals(Tx.transaction(log)(tx => tx.execute("a") + tx.execute("b")), 2)
    assertEquals(Tx(log) { Tx.execute("c") }, 1)
    assertEquals(intercept[IllegalStateException](Tx.transaction(log)(_ => throw IllegalStateException("no"))).getMessage, "no")
    assertEquals(log.said.toList, List("begin", "a", "b", "commit", "begin", "c", "commit", "begin", "rollback"))
  }

  test("compiles: the transaction used inside, its results out") {
    assertEquals(refused("val n = Tx.transaction(db)(tx => tx.execute(\"a\") + tx.execute(\"b\"))"), Nil)
    assertEquals(refused("val n = Tx(db) { Tx.execute(\"a\") }"), Nil)
    assertEquals(refused("val ns = Tx.transaction(db)(tx => List(1, 2).map(_ => tx.execute(\"a\")))"), Nil)
  }

  test("refused: the transaction, or what holds it, out of its bracket") {
    for code <- List(
      "val t = Tx.transaction(db)(tx => tx)",
      "val f = Tx.transaction(db)(tx => () => tx.execute(\"a\"))",
      "val g = Tx(db) { () => Tx.execute(\"a\") }",
      "val fs = Tx.transaction(db)(tx => List(1).map(_ => () => tx.execute(\"a\")))",
      "val it = Tx.transaction(db)(tx => Iterator(1, 2).map(_ => tx.execute(\"a\")))",
    ) do
      val es = refused(code)
      assert(es.exists(_.contains(outlives)), s"$code: $es")
    val kept = refused("var keep: Tx | Null = null\nval u = Tx.transaction(db)(tx => { keep = tx; 1 })")
    assert(kept.nonEmpty, kept)
  }

  test("the holes, pinned: each compiles today — a test that fails the day one is closed") {
    // a caller without capture checking: the guarantee is the caller's to switch on
    assertEquals(Compile.errors(db + "val f = Tx.transaction(db)(tx => () => tx.execute(\"a\"))", captureChecking = false), Nil)
    // a LazyList over it: the standard library's LazyList.map does not track what its function holds
    assertEquals(refused("val l = Tx.transaction(db)(tx => LazyList.from(1).map(_ => tx.execute(\"a\")))"), Nil)
    // a kernel program holding it: okay-core is not capture-checked, its closures carry no capture set
    assertEquals(refused("val p = Tx.transaction(db)(tx => okay.one.Cont.pure[okay.one.row.Pure, Int](1).map(_ => tx.execute(\"a\")))"), Nil)
  }
