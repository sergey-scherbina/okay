package okay.sql

import okay.{!, Async, Chunk, Prog}

/**
 * THE TRANSACTION PROTOCOL IN THE TYPES — as a program (specs/freer-
 * base.md, stage 2; the module protocol the roadmap asked for).
 *
 * `Sql.begin`/`commit`/`rollback` are three programs whose ORDER is
 * the protocol, and today the order is checked at run time: `PgSql.
 * begin` inside a transaction throws `IllegalStateException("nested
 * transaction")`, a `commit` with no `begin` is a server error, and a
 * program that ends inside a transaction leaves the connection for
 * the next caller to find. `Typed.region` closes the nested case with
 * a phantom on the HANDLE (`Db[Tx.No]`/`Db[Tx.Yes]`); this closes all
 * three on the PROGRAM, with the indexes of `okay.Prog`:
 *
 *     val tx = Tx(db)
 *     Tx.run:                                    // only Idle -> Idle runs
 *       tx.begin().flatMap { g =>
 *         tx.update("insert into t values (1)")  // Open -> Open
 *           .flatMap(_ => tx.commit())           // Open -> Idle
 *       }
 *
 * `tx.begin().flatMap(_ => tx.begin())` does not compile (`begin`
 * starts at `Idle`, the continuation is at `Open`); `tx.commit()`
 * alone cannot be run (`Open -> Idle` is not closed); `tx.begin()`
 * alone has no `free`. The runtime is EXACTLY `Sql`'s: every step is
 * the driver's own program, unchanged, and the indexes erase.
 *
 * Every transition is made with `Prog.transition` HERE and nowhere
 * else — the claim a protocol's author signs, kept in the smart
 * constructors so that a call site can only compose them.
 */
final class Tx(db: Sql):
  import Tx.{Idle, Open, Step}

  /** BEGIN, with the isolation the server granted — `Idle -> Open` */
  def begin(isolation: Isolation = Isolation.ReadCommitted, readOnly: Boolean = false): Step[Granted, Idle, Open] =
    Prog.transition[Idle, Open, Async, Granted](db.begin(isolation, readOnly))

  /** COMMIT — `Open -> Idle` (a commit that answered ROLLBACK still
   * throws, as `Sql.commit` documents; the index does not promise
   * durability, only order) */
  def commit(): Step[Unit, Open, Idle] =
    Prog.transition[Open, Idle, Async, Unit](db.commit())

  /** ROLLBACK — `Open -> Idle` */
  def rollback(): Step[Unit, Open, Idle] =
    Prog.transition[Open, Idle, Async, Unit](db.rollback())

  /** a statement runs in either state and moves nothing */
  def update[S](sql: String, params: Vector[SqlValue] = Vector.empty): Step[Long, S, S] =
    Prog.diag[S, Async, Long](db.update(sql, params))

  def batch[S](sql: String, rows: Chunk[Vector[SqlValue]]): Step[Long, S, S] =
    Prog.diag[S, Async, Long](db.batch(sql, rows))

  def describe[S](sql: String): Step[Vector[Col], S, S] =
    Prog.diag[S, Async, Vector[Col]](db.describe(sql))

object Tx:
  /** outside a transaction */
  sealed trait Idle
  /** inside one */
  sealed trait Open

  /** one step of the protocol: a driver program, indexed */
  type Step[A, S, R] = Prog[Async, A, S, R]

  /** run a CLOSED program — one that begins and ends outside a
   * transaction. Anything else is not this method's type. */
  def run[A](p: Step[A, Idle, Idle]): A ! Async = p.free
