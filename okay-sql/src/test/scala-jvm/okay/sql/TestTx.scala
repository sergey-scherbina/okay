package okay.sql

import okay.{!, Async, Chunk, pure, Source}
import okay.given
// the facade's own extensions, imported: found only through `Prog`'s
// companion, `flatMap` does not infer its continuation's index
// (`Required: Prog.Rep[Async, B, R, T]`, B and T uninstantiated) — the
// documented answer (docs/guide.md, "Typestate on a program"). It was
// once blamed on a package-level `Comonad[Id]` capturing `.map`; that
// given moved to `Comonad`'s companion (comonad-id-map-capture) and
// this import is still needed.
import okay.Prog.{flatMap, map}
import scala.collection.mutable.ArrayBuffer

/**
 * specs/freer-base.md, stage 2, the module protocol: the transaction
 * order in the types, over any `Sql`. A recording fake stands in for
 * a driver — the protocol is about ORDER, and order is what it logs.
 * JVM: running an `Async` program takes `CanBlock`, which JS lacks.
 */
class TestTx extends munit.FunSuite:

  /** every step recorded, none performed */
  final class Recording extends Sql:
    val log = ArrayBuffer.empty[String]
    def describe(sql: String): Vector[Col] ! Async = { log += s"describe $sql"; pure(Vector.empty) }
    def query(sql: String, params: Vector[SqlValue] = Vector.empty): Source[Chunk[Vector[SqlValue]]] =
      throw UnsupportedOperationException("not a step")
    def update(sql: String, params: Vector[SqlValue] = Vector.empty): Long ! Async = { log += s"update $sql"; pure(1L) }
    def batch(sql: String, rows: Chunk[Vector[SqlValue]]): Long ! Async = { log += s"batch $sql"; pure(rows.size.toLong) }
    def begin(isolation: Isolation, readOnly: Boolean = false): Granted ! Async =
      { log += s"begin $isolation"; pure(Granted(isolation, isolation, readOnly)) }
    def commit(): Unit ! Async = { log += "commit"; pure(()) }
    def rollback(): Unit ! Async = { log += "rollback"; pure(()) }
    def cancel(): Unit = ()

  test("a well-bracketed program runs its steps in the order the type promised") {
    val db = Recording()
    val tx = Tx(db)
    val n = Tx.run(
      tx.begin().flatMap { g =>
        tx.update("insert into t values (1)").flatMap(_ => tx.commit()).map(_ => g.granted)
      }).runWith
    assertEquals(n, Isolation.ReadCommitted)
    assertEquals(db.log.toList, List("begin ReadCommitted", "update insert into t values (1)", "commit"))
  }

  test("a statement outside any transaction is a closed program too") {
    val db = Recording()
    val tx = Tx(db)
    assertEquals(Tx.run(tx.update[Tx.Idle]("select 1")).runWith, 1L)
    assertEquals(db.log.toList, List("update select 1"))
  }

  test("the three run-time refusals are compile errors: nested begin, commit with no begin, a program left open") {
    val nested = compileErrors("""
      { val tx = okay.sql.Tx(new okay.sql.TestTx().Recording())
        tx.begin().flatMap(_ => tx.begin()) }""")
    assert(nested.nonEmpty, "a nested begin compiled — PgSql.begin would throw IllegalStateException")
    val orphan = compileErrors("""
      { val tx = okay.sql.Tx(new okay.sql.TestTx().Recording())
        okay.sql.Tx.run(tx.commit()) }""")
    assert(orphan.nonEmpty, "a commit with no begin compiled")
    val open = compileErrors("""
      { val tx = okay.sql.Tx(new okay.sql.TestTx().Recording())
        okay.sql.Tx.run(tx.begin().flatMap(_ => tx.update("insert"))) }""")
    assert(open.nonEmpty, "a program that ends inside a transaction compiled")
    val unlifted = compileErrors("""
      { val tx = okay.sql.Tx(new okay.sql.TestTx().Recording())
        tx.begin().free }""")
    assert(unlifted.nonEmpty, "an open move was unlifted")
  }
