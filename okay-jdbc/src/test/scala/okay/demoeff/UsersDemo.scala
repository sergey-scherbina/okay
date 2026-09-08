package okay.demoeff

/**
 * The worked example behind the "define your own effect" note: one
 * program, four interpretations, nothing mocked.
 *
 * `rename` says in its TYPE everything it can do — look users up,
 * store them, and STOP if there is nobody there — and nothing else.
 * The four handlers below answer those operations from a real SQLite
 * file, from a Map, from State and Writer, and (with `.tracing`) into
 * a log. The program between them is byte-identical.
 *
 * The handlers here issue plain JDBC deliberately: a `Handler` must
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
import okay.Direct.{direct, given}
import scala.language.implicitConversions

/**
 * The whole declaration of an effect: the operations, their answer
 * types, and two names.
 *
 * `derives Effect` writes the instance a row split needs — it is the
 * hand-written `typeableK(classOf[Users[?]])` with the class no
 * longer spelled out, under a name that says what is being declared
 * rather than which mechanism does it.
 *
 * The two constructors are OPTIONAL: `Users.Find(id).perform` says
 * the same thing with nothing declared. They are here because they
 * are this effect's API — one line each, and every call site reads
 * better for them.
 */
enum Users[+A] derives Effect:
  case Find(id: Long) extends Users[Option[String]]
  case Save(id: Long, name: String) extends Users[Unit]

object Users:
  inline def find(id: Long): Option[String] ! Users = effect(Find(id))
  inline def save(id: Long, name: String): Unit ! Users = effect(Save(id, name))

object UsersDemo:

  /**
   * Renaming somebody who is not there is not a rename, and the first
   * cut of this got it wrong: `find` then `save` in a
   * for-comprehension SEQUENCES and does not branch, so a missing id
   * reached the handler's upsert and CREATED the user. The demo
   * printed the bug itself — "row 99 is now hopper".
   *
   * The fix is not a fold. It is a pattern, plus a row that says this
   * program MAY STOP. `case Some(old) <-` desugars to `withFilter`,
   * `withFilter` needs somewhere for the dropped step to go, and
   * `Abort` — failure carrying no information, which is all a missing
   * row has to say — is that somewhere (Fail.scala).
   *
   * So `save` cannot run for a missing id because it is NOT REACHABLE,
   * not because a branch remembered to skip it. The Option is gone
   * from the middle of the program and comes back at the end, as
   * `runOption`'s answer.
   */
  def rename(id: Long, to: String): Option[String] ! Users = runOption {
    for
      case Some(old) <- Users.find(id).plus[Abort]
      _              <- Users.save(id, to).plus[Abort]
    yield old
  }

  /**
   * The same effect in DIRECT style, where an effect stands in the
   * place of its answer with no marks at all.
   *
   * Two things color here and they come from different places. A
   * PROGRAM (`Users.find(a)`, an `Option[String] ! Users`) colors
   * because the block declares its carrier. An OPERATION
   * (`Users.Find(b)`, typed at the enum) colors because `derives
   * Effect` registered this signature for it. Neither colors OUTSIDE
   * a direct block: the conversion needs a capability that exists
   * only inside one, so `F[A]`-as-`A` stays the compile error it
   * always was everywhere else.
   */
  def initials(a: Long, b: Long): (Option[Char], Option[Char]) ! Users = direct {
    val first: Option[String] = Users.find(a)
    val second: Option[String] = (Users.Find(b): Users[Option[String]])
    (first.map(_.head), second.map(_.head))
  }


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
   * `Writer.tell` does not fit beside `State.get` — and `.at[R]`
   * moves each into the row they share, naming the target and never
   * the complement (okay.Rowlift; one cast, measured at the same
   * B/op as constructing the operation at R).
   *
   * `.at` and not `.plus` here, which is the whole rule: `plus` is
   * the one to reach for, but it needs the target to have the form
   * "my row plus something", and R here is `Tracked + F` with F
   * abstract — a row known only by membership.
   *
   * The outer `!.widen` stays: it moves a whole program rather than
   * one operation, and a walk over a program is also a normalisation.
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

      println("PROD  " + rename(7L, "grace").runWith(using live(c)) +
              " / row 7 is now " + nameOf(c, 7L))

      val state = scala.collection.mutable.Map(7L -> "ada")
      val log = scala.collection.mutable.ListBuffer[Any]()
      println("TEST  " + rename(7L, "grace").runWith(using inMemory(state).tracing(log += _)) +
              " / log=" + log.mkString(", ") + " / state=" + state)

      // the id nobody has: the database is untouched, and the trace
      // shows WHY — a find and no save. Note WHICH handler is traced:
      // the SQLite one. Recording is not a test-only trick.
      val missLog = scala.collection.mutable.ListBuffer[Any]()
      val missLive = rename(99L, "hopper").runWith(using live(c).tracing(missLog += _))
      val missTest = rename(99L, "hopper").runWith(
        using inMemory(scala.collection.mutable.Map()))
      println(s"MISS  $missLive / row 99 is now ${nameOf(c, 99L)}" +
              s" / both worlds agree: ${missTest == missLive}" +
              s" / log=${missLog.mkString(", ")}")

      println("DIRECT " + initials(7L, 99L).runWith(using live(c)))

      // no mutable collection anywhere: the store is State, the log is
      // Writer, and the run answers with all three as plain data
      val (store, (told, answer)) =
        State.run[Store, (Seq[String], Option[String])](Map(7L -> "ada"))(
          Writer.run[String, Option[String], State % Store](tracked(rename(7L, "grace"))))
      println(s"PURE  $answer / log=${told.mkString(", ")} / store=$store")
    finally c.close()
