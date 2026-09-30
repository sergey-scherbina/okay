package okay.sql

import okay.{!, Async, Chunk, Freer, Indexed, TypeableI, Unary, +~, splitI}
import scala.annotation.tailrec

/**
 * THE TRANSACTION PROTOCOL IN THE TYPES, AS DATA (specs/indexed-effects.md,
 * stages 2 and 8). `Sql.begin`/`commit`/`rollback` are three programs
 * whose ORDER is the protocol; the order used to be checked at run time
 * (`PgSql.begin` inside a transaction throws, a `commit` with no `begin`
 * is a server error, a program that ends inside a transaction leaves the
 * connection for the next caller). Here the transitions are on the
 * signature `TxOp`, the tree carries them along every `flatMap`, and the
 * handler `interpret` holds the connection typed by the index:
 *
 *     Tx.interpret(
 *       Tx.begin().flatMap { g =>
 *         Tx.update[Tx.Open]("insert into t values (1)")   // Open -> Open
 *           .flatMap(_ => Tx.commit())                     // Open -> Idle
 *           .map(_ => g.granted)
 *       })(db)
 *
 * `begin().flatMap(_ => begin())` does not compile, `interpret(commit())`
 * does not, a program that ends inside a transaction does not. The
 * `Prog` facade that carried this as a phantom claim (`Prog.transition`,
 * freer-base stage 2) is gone: the data road is THE road.
 */
object Tx:
  /** outside a transaction */
  sealed trait Idle
  /** inside one */
  sealed trait Open

  /**
   * THE PROTOCOL AS DATA (specs/indexed-effects.md, stage 2): the
   * transitions said ONCE in a signature the compiler checks along
   * every `Bind` —
   * `Begin` moves `Idle` to `Open`, `Commit`/`Rollback` move back, a
   * statement moves nothing. The tree carries the index, so nothing
   * is claimed at a call site and nothing is erased.
   *
   * WHAT THE DATA ROAD ADDS over the facade is in `interpret`: the
   * handler holds the CONNECTION typed by the index, `Conn[Idle]` or
   * `Conn[Open]`, and moves it only in the arm that runs the driver's
   * `begin`/`commit`/`rollback` — a `commit` from a `Conn[Idle]` does
   * not type even inside the handler (TestTxData pins it). The facade
   * keeps its spelling; this is the second door beside it.
   *
   * The body is not the protocol alone: the row is `TxOp +~
   * Unary[Async]`, so a transaction body may run any `Async` program
   * (a wait, another service, a log) through `Data.async`, on the
   * diagonal, and `interpret` forwards it into the program it builds.
   * The reading of the two indexes is McBride's — `R` the state before
   * the operation, `S` after — on the base made invariant for it.
   */
  enum TxOp[S, R, +X]:       // the tree's order: `R` the state before, `S` after
    case Begin(isolation: Isolation, readOnly: Boolean) extends TxOp[Open, Idle, Granted]
    case Commit() extends TxOp[Idle, Open, Unit]
    case Rollback() extends TxOp[Idle, Open, Unit]
    case Update[S](sql: String, params: Vector[SqlValue]) extends TxOp[S, S, Long]
    case Batch[S](sql: String, rows: Chunk[Vector[SqlValue]]) extends TxOp[S, S, Long]
    case Describe[S](sql: String) extends TxOp[S, S, Vector[Col]]

  given TypeableI[TxOp] = TypeableI.derived

  /** the transaction row: the protocol, and any `Async` beside it on the diagonal */
  type Row = TxOp +~ Unary[Async]

  /** a transaction program on the data road, READ LEFT TO RIGHT: `A`
   * computed, the connection's state moved from `From` to `To`. The
   * tree spells the pair the other way round (`Freer[G, after, before,
   * A]`, Free.scala's header), and this alias is where the two
   * spellings meet — `Data[Granted, Idle, Open]` is `begin`. */
  type Data[A, From, To] = Freer[Row, To, From, A]

  /**
   * THE CONNECTION, TYPED BY THE INDEX. A phantom on the handle, as
   * `Typed.region`'s `Db[Tx.Yes]` is — but moved only by the two arms
   * of `interpret` that run the driver's own transition, so the type
   * of the value in the handler's hand is the state the program's index
   * says the connection is in.
   */
  final class Conn[S] private[sql] (val db: Sql)
  object Conn:
    extension (c: Conn[Idle]) private[sql] def opened: Conn[Open] = new Conn[Open](c.db)
    extension (c: Conn[Open]) private[sql] def closed: Conn[Idle] = new Conn[Idle](c.db)

  import Conn.{opened, closed}

  /** BEGIN — `Idle -> Open`, with the isolation the server granted */
  def begin(isolation: Isolation = Isolation.ReadCommitted, readOnly: Boolean = false): Data[Granted, Idle, Open] =
    Indexed.effect[Row, Open, Idle, Granted](TxOp.Begin(isolation, readOnly))
  /** COMMIT — `Open -> Idle` */
  def commit(): Data[Unit, Open, Idle] = Indexed.effect[Row, Idle, Open, Unit](TxOp.Commit())
  /** ROLLBACK — `Open -> Idle` */
  def rollback(): Data[Unit, Open, Idle] = Indexed.effect[Row, Idle, Open, Unit](TxOp.Rollback())
  /** a statement runs in either state and moves nothing: the diagonal node */
  def update[S](sql: String, params: Vector[SqlValue] = Vector.empty): Data[Long, S, S] =
    Indexed.unary[Row, S, Long](TxOp.Update(sql, params))
  def batch[S](sql: String, rows: Chunk[Vector[SqlValue]]): Data[Long, S, S] =
    Indexed.unary[Row, S, Long](TxOp.Batch(sql, rows))
  def describe[S](sql: String): Data[Vector[Col], S, S] =
    Indexed.unary[Row, S, Vector[Col]](TxOp.Describe(sql))
  /** any `Async` program inside the body, in either state: the
   * transaction is not the whole of what a body does */
  def async[S, A](p: A ! Async): Data[A, S, S] =
    Indexed.lift[Row, Async, S, A](p)([X] => (e: Async[X]) => e)

  /**
   * Run a CLOSED program against a driver: the handler threads
   * `Conn[S]` and every operation is the driver's own program,
   * unchanged, flatMapped into the answer — `State.handle`'s shape,
   * the output an `Async` program. The type says `Idle -> Idle`; an
   * abort inside the body still drops the continuation and the
   * `commit` with it (TestTxData asserts the caveat), so a body that
   * may fail is bracketed by its caller as the facade's is.
   */
  def interpret[A](p: Data[A, Idle, Idle])(db: Sql): A ! Async = loop(new Conn[Idle](db))(p)

  /** a statement on the diagonal: the driver's program for it. The
   * three transitions cannot sit at `TxOp[S, S, X]` — their indexes
   * differ — and `@unchecked` says so once instead of three dead arms */
  private def statement[S, X](c: Conn[S], op: TxOp[S, S, X]): X ! Async = (op: @unchecked) match
    case TxOp.Update(sql, params) => c.db.update(sql, params)
    case TxOp.Batch(sql, rows) => c.db.batch(sql, rows)
    case TxOp.Describe(sql) => c.db.describe(sql)

  /** the loop re-entered from under a `flatMap`: a call there is not
   * a tail call, and `@tailrec` must not see it as one (State.handle's
   * `_loop`) */
  private def again[T, R, A](c: Conn[R])(p: Freer[Row, T, R, A]): A ! Async = loop(c)(p)

  /** in the tree's order: `R` the connection's state now, `T` after
   * the program, `c: Conn[R]` the value of that state */
  @tailrec private def loop[T, R, A](c: Conn[R])(p: Freer[Row, T, R, A]): A ! Async = (p.resume: @unchecked) match
    case Freer.Return(a) => okay.pure(a)
    case Freer.Diag(e) => loop(c)(Freer.Diag[Row, R, A](e).flatMap(v => Freer.Return(v)))
    case Freer.Inject(o) => loop(c)(Freer.Inject[Row, T, R, A](o).flatMap(v => Freer.Return(v)))
    case Freer.Bind(Freer.Diag(e), k) =>
      splitI[TxOp, Unary[Async]](e)(op => statement(c, op).flatMap(x => again(c)(k(x))))(a =>
        okay.Free.inject(a).flatMap(x => again(c)(k(x))))
    case Freer.Bind(Freer.Inject(o), k) => splitI[TxOp, Unary[Async]](o) {
        // the GADT binds the connection's state to the operation's:
        // `Begin` is `TxOp[Open, Idle, Granted]`, so here `c: Conn[Idle]`
        // and `opened` exists; a `Commit` arm from `Conn[Idle]` would not
        // type (TestTxData pins it). A statement may sit under `Inject`
        // too, at the same state on both sides.
        case TxOp.Begin(iso, ro) => c.db.begin(iso, ro).flatMap(g => again(c.opened)(k(g)))
        case TxOp.Commit() => c.db.commit().flatMap(u => again(c.closed)(k(u)))
        case TxOp.Rollback() => c.db.rollback().flatMap(u => again(c.closed)(k(u)))
        case TxOp.Update(sql, params) => c.db.update(sql, params).flatMap(x => again(c)(k(x)))
        case TxOp.Batch(sql, rows) => c.db.batch(sql, rows).flatMap(x => again(c)(k(x)))
        case TxOp.Describe(sql) => c.db.describe(sql).flatMap(x => again(c)(k(x)))
      }(Indexed.offDiagonal)
