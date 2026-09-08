package okay.demoeff

/**
 * The worked example behind the "define your own effect" note: one
 * program, two interpretations, nothing mocked.
 *
 * `rename` says in its TYPE that it looks up and stores users and
 * does nothing else, and it answers with whatever name was there
 * before — Option, so the id nobody has needs no invented
 * placeholder to keep the signature honest.
 *
 * `live` is a REAL database: a SQLite file, a select and an upsert.
 * `recording` keeps state in memory AND records every operation,
 * which is what turns "what did this ask for, and in what order"
 * into an assertion rather than a debugging session. The program
 * between them is byte-identical.
 *
 * The handler here issues plain JDBC deliberately: a `Handler` must
 * ANSWER with a value, so anything effectful inside it has to be run
 * at that point. When you want the query itself to stay an effect —
 * streamed, transacted, typed by a Schema — that is okay-jdbc's Sql
 * seam (`JdbcSql`, `Typed.rows`, `Typed.transact`), and then the row
 * type joins the program's own signature instead of being run here.
 *
 * Runnable, and its output is what the note quotes:
 *   sbt "okayJdbc/Test/runMain okay.demoeff.UsersDemo"
 */

import okay.*
import okay.given
import java.sql.{Connection, DriverManager}
import okay.Rowlift.{at, plus}

/**
 * The whole declaration of an effect: the operations, their answer
 * types, and two names.
 *
 * `derives TypeableK` writes the instance a row split needs — it is
 * the hand-written `typeableK(classOf[Users[?]])` with the class no
 * longer spelled out.
 *
 * The two constructors are OPTIONAL: `Users.Find(id).perform` says
 * the same thing with nothing declared. They are here because they
 * are this effect's API — one line each, and every call site reads
 * better for them.
 */
enum Users[+A] derives TypeableK:
  case Find(id: Long) extends Users[Option[String]]
  case Save(id: Long, name: String) extends Users[Unit]

object Users:
  inline def find(id: Long): Option[String] ! Users = effect(Find(id))
  inline def save(id: Long, name: String): Unit ! Users = effect(Save(id, name))

object UsersDemo:

  /**
   * Renaming somebody who is not there is not a rename — and the first
   * cut of this program got it wrong in a way worth keeping on the
   * record. It said `find` then `save` in a for-comprehension, which
   * SEQUENCES and does not branch, so a missing id still reached the
   * handler's upsert and CREATED the user; the answer `None` then
   * meant two things at once, "no previous name" and "nothing
   * written". The demo printed the bug itself: "row 99 is now hopper".
   *
   * The fix is not a fold. It is a pattern, and the row saying that
   * this program MAY STOP: `Abort` is failure carrying no information,
   * which is all a missing row has to say. `case Some(old) <-`
   * desugars to `withFilter`, `withFilter` needs somewhere for the
   * dropped step to go, and `Abort` in the row is that somewhere
   * (Fail.scala).
   *
   * So the Option leaves the signature — `String`, not
   * `Option[String]` — and comes back at the END, as `runOption`'s
   * answer. `save` cannot run for a missing id because it is not
   * reachable, not because a branch remembered to skip it.
   */
  def rename(id: Long, to: String): String ! (Users + Abort) =
    for
      case Some(old) <- Users.find(id).plus[Abort]
      _              <- Users.save(id, to).plus[Abort]
    yield old

  /** the same program with its answer back in a value */
  def renamed(id: Long, to: String): Option[String] ! Users =
    runOption[String, Users](rename(id, to).at[Abort + Users])


  /** the real world: a SQLite file */
  def live(c: Connection): Handler[Users] = new:
    def handle[A](e: Users[A]): A = e match
      case Users.Find(id) =>
        val ps = c.prepareStatement("select name from users where id = ?")
        try
          ps.setLong(1, id)
          val rs = ps.executeQuery()
          if rs.next() then Some(rs.getString(1)) else None
        finally ps.close()
      case Users.Save(id, name) =>
        val ps = c.prepareStatement(
          "insert into users(id, name) values (?, ?) " +
          "on conflict(id) do update set name = excluded.name")
        try { ps.setLong(1, id); ps.setString(2, name); ps.executeUpdate(); () }
        finally ps.close()

  /**
   * The test world: the same program, no database. It does NOT
   * record — recording is `.tracing`, which any handler can wear,
   * including the SQLite one below. The operations are already data,
   * so "what did this ask for, and in what order" needs a decorator,
   * not a second handler that might drift from the first.
   */
  def inMemory(state: scala.collection.mutable.Map[Long, String]): Handler[Users] = new:
    def handle[A](e: Users[A]): A = e match
      case Users.Find(id)       => state.get(id)
      case Users.Save(id, name) => state(id) = name; ()

  /**
   * The test world WITHOUT mutable state: the same operations
   * interpreted into OTHER EFFECTS rather than into values.
   *
   * A `Handler` answers with a value, so it cannot tell or get — but
   * `translate` interprets each operation into a PROGRAM in the
   * target row, and there State and Writer are ordinary members. The
   * store becomes `State % Map`, the log becomes `Writer % String`,
   * and the residual row F is whatever the caller was already doing.
   */
  type Store = Map[Long, String]
  type Tracked = State % Store + Writer % String

  /**
   * A for-comprehension fixes its row from the first step, so
   * `Writer.tell` (row `Writer % String`) does not fit beside
   * `State.get` (row `State % Store`) — and the first draft of this
   * file worked around that with three helpers wrapping `!.widen`,
   * each naming the COMPLEMENT of the row it was widening into. That
   * is the roughness `.at` exists to remove.
   *
   * `p.at[R]` moves a program into any row R that CONTAINS its own.
   * The target is named; the complement never is. Under the hood it
   * is one cast licensed by a witness — `+` is a union and unions
   * erase, so a program in `State % Store` already IS a program in R
   * (okay.Rowlift). It costs nothing: measured at the same B/op as
   * constructing the operation at R in the first place.
   *
   * Two other spellings say the same thing and are still fine:
   *
   *   effect[R, Store](State.Get())      the operation injected
   *                                      straight into row R
   *
   *   direct { State.Get[Store, Store]().!? }
   *                                      the macro reads the row off
   *                                      the block's expected type
   *
   * `.at` is the one that keeps the SMART constructors — `State.get`,
   * `Writer.tell` — which is what makes the block below read like the
   * single-effect code it is.
   *
   * The outer `!.widen` stays: it moves the whole program, not one
   * operation, and a walk over a program is also a normalisation
   * (specs/writer-covariance.md).
   */
  def tracked[A, F[+_]](prog: A ! (Users + F)): A ! (Tracked + F) =
    type R = Tracked + F
    val widened: A ! (Users + R) = !.widen[A, Users + F, Tracked](prog)
    !.translate[A, Users, R](widened):
      [X] => (e: Users[X]) => e match
        case Users.Find(id) =>
          val p: Option[String] ! R =
            for
              m <- State.get[Store].at[R]
              _ <- Writer.tell(s"find($id)").at[R]
            yield m.get(id)
          p.map[X](x => x)
        case Users.Save(id, name) =>
          val p: Unit ! R =
            for
              m <- State.get[Store].at[R]
              _ <- State.set(m + (id -> name)).at[R]
              _ <- Writer.tell(s"save($id,$name)").at[R]
            yield ()
          p.map[X](x => x)

  private def nameOf(c: Connection, id: Long): String =
    val rs = c.createStatement().executeQuery(s"select name from users where id = $id")
    if rs.next() then rs.getString(1) else "-"

  def main(args: Array[String]): Unit =
    val file = java.nio.file.Files.createTempDirectory("okay-demoeff").resolve("users.db")
    val c = DriverManager.getConnection(s"jdbc:sqlite:$file")
    try
      val st = c.createStatement()
      st.execute("create table users(id integer primary key not null, name text not null)")
      st.execute("insert into users values (7, 'ada')")
      st.close()

      println("PROD  " + renamed(7L, "grace").runWith(using live(c)) +
              " / row 7 is now " + nameOf(c, 7L))

      val state = scala.collection.mutable.Map(7L -> "ada")
      val log = scala.collection.mutable.ListBuffer[Any]()
      println("TEST  " + renamed(7L, "grace").runWith(using inMemory(state).tracing(log += _)) +
              " / log=" + log.mkString(", ") + " / state=" + state)

      // the id nobody has: the database is untouched, and the trace
      // shows WHY — a find and no save. Note WHICH handler is traced:
      // the SQLite one. Recording is not a test-only trick.
      val missLog = scala.collection.mutable.ListBuffer[Any]()
      val missLive = renamed(99L, "hopper").runWith(using live(c).tracing(missLog += _))
      val missTest = renamed(99L, "hopper").runWith(
        using inMemory(scala.collection.mutable.Map()))
      println(s"MISS  $missLive / row 99 is now ${nameOf(c, 99L)}" +
              s" / both worlds agree: ${missTest == missLive}" +
              s" / log=${missLog.mkString(", ")}")

      // no mutable collection anywhere: the store is State, the log is
      // Writer, and the run answers with all three as plain data
      val (store, (told, answer)) =
        State.run[Store, (Seq[String], Option[String])](Map(7L -> "ada"))(
          Writer.run[String, Option[String], State % Store](tracked(renamed(7L, "grace"))))
      println(s"PURE  $answer / log=${told.mkString(", ")} / store=$store")
    finally c.close()
