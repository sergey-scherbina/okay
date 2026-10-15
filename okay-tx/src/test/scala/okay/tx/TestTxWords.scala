package okay.tx

import okay.core.*
import okay.core.std.*

/** transactions in the facade's words: brackets nested through functions that do not know each other */
class TestTxWords extends munit.FunSuite:
  type T = Writer % String +: Pure
  type Conn = String
  /** inside a bracket: the transaction's effect again, the resource, the row outside */
  type B = Txs[Conn, T] +: Reader % Conn +: T
  object owner extends Tx.Transactor[Conn, T]:
    def begin: Eff[T, Conn] = tell("BEGIN").at[T].map(_ => "c1")
    def commit(c: Conn): Eff[T, Unit] = tell(s"COMMIT $c").at[T]
    def rollback(c: Conn): Eff[T, Unit] = tell(s"ROLLBACK $c").at[T]

  def sql(s: String): Eff[B, Unit] = ask[Conn].at[B].flatMap(c => tell(s"$s on $c").at[B])
  def charge: Eff[B, Unit] = atomic[Conn, T, Unit](sql("update").flatMap(_ => sql("insert payment"))).at[B]
  def place: Eff[Txs[Conn, T] +: T, Unit] =
    atomic[Conn, T, Unit](sql("insert order").flatMap(_ => charge)).at[Txs[Conn, T] +: T]

  test("place and charge, each its own bracket: one transaction"):
    val r = run(writer[String, Unit, Pure](transactions[Conn, Unit, T](owner)(place)))
    assertEquals(r, (List("BEGIN", "insert order on c1", "update on c1", "insert payment on c1", "COMMIT c1"), ()))
