package okay.tx

import okay.core.*
import okay.core.std.*

/** THE RULE of docs/modules/okay-caps.md, tested: a capability kept an EFFECT — the bracket's resource, an operation
 * of its row — cannot leave the bracket: performed outside, or a program performing it carried out, is a type error.
 * Where it becomes a VALUE and a closure holds it, it leaves: the boundary the capture checker or a guard covers */
class TestTxEscape extends okay.testkit.Munit.Diagnosed:
  type T = Writer % String +: Pure
  type Conn = String
  type B = Txs[Conn, T] +: Reader % Conn +: T
  object owner extends Tx.Transactor[Conn, T]:
    def begin: Eff[T, Conn] = tell("BEGIN").at[T].map(_ => "c1")
    def commit(c: Conn): Eff[T, Unit] = tell(s"COMMIT $c").at[T]
    def rollback(c: Conn): Eff[T, Unit] = tell(s"ROLLBACK $c").at[T]

  def sql(s: String): Eff[B, Unit] = ask[Conn].at[B].flatMap(c => tell(s"$s on $c").at[B])

  test("the resource performed outside any bracket: a type error — the row outside has no reader of it") {
    val errors = compileErrors("val leak: Eff[T, Conn] = ask[Conn].at[T]")
    note(errors)
    // no path to a reader of the resource in the row outside: no handler of it there
    assert(errors.contains("No given instance of type okay.core.Path[okay.core.std.Reader % TestTxEscape.this.Conn"), errors)
  }

  test("a program performing the resource, carried out of its bracket and run there: a type error") {
    val errors = compileErrors(
      """val carried: Eff[Txs[Conn, T] +: T, Unit] = atomic[Conn, T, Eff[B, Unit]](sql("inside").map(_ => sql("late"))).at[Txs[Conn, T] +: T].flatMap(p => p)""")
    note(errors)
    // the carried program's row is the bracket's (B), not the row outside
    assert(errors.contains("Found:    (p : okay.core.Eff[TestTxEscape.this.B, Unit])"), errors)
  }

  test("the boundary: the resource read into a VALUE and closed over leaves the bracket — the checker's or a guard's") {
    val leaked: Eff[Txs[Conn, T] +: T, () => Conn] =
      atomic[Conn, T, () => Conn](ask[Conn].at[B].map(c => () => c)).at[Txs[Conn, T] +: T]
    val (log, f) = run(writer[String, () => Conn, Pure](transactions[Conn, () => Conn, T](owner)(leaked)))
    assertEquals(log, List("BEGIN", "COMMIT c1"))
    assertEquals(f(), "c1") // the connection, after its commit
  }
