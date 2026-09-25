package okay2.sql

import okay2.{!, Prog}
import okay2.async.Async
import okay2.stream.Chunk

/**
 * A transaction's protocol as a TYPESTATE (okay-sql's Tx.scala): begin
 * moves Idle -> Open, commit and rollback Open -> Idle, statements keep
 * the state, and `Tx.run` takes only an Idle -> Idle program — a begin
 * left open, a commit outside a transaction, a second begin are compile
 * errors, at no run-time cost (a `Prog` is its `Free`).
 */
final class Tx(db: Sql) {
  import Tx.{Idle, Open, Step}

  def begin(isolation: Isolation = Isolation.ReadCommitted, readOnly: Boolean = false): Step[Granted, Idle, Open] =
    Prog.transition[Idle, Open, Async, Granted](db.begin(isolation, readOnly))

  def commit(): Step[Unit, Open, Idle] = Prog.transition[Open, Idle, Async, Unit](db.commit())

  def rollback(): Step[Unit, Open, Idle] = Prog.transition[Open, Idle, Async, Unit](db.rollback())

  def update[S](sql: String, params: Vector[SqlValue] = Vector.empty): Step[Long, S, S] =
    Prog.diag[S, Async, Long](db.update(sql, params))

  def batch[S](sql: String, rows: Chunk[Vector[SqlValue]]): Step[Long, S, S] =
    Prog.diag[S, Async, Long](db.batch(sql, rows))

  def describe[S](sql: String): Step[Vector[Col], S, S] =
    Prog.diag[S, Async, Vector[Col]](db.describe(sql))
}

object Tx {
  sealed trait Idle
  sealed trait Open

  type Step[A, S, R] = Prog[Async, A, S, R]

  def run[A](p: Step[A, Idle, Idle]): A ! Async = Prog.free(p)
}
