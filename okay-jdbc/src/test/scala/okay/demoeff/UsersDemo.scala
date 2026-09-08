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
import okay.RowLift.plus
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
   * WHAT A STORE IS, said once: something a name can be read out of,
   * and put back INTO — where "back into" ANSWERS a new store rather
   * than mutating this one.
   *
   * That immutability is the whole reason this is a class and not a
   * `Map`. A store that answers its successor can be held in a `var`
   * by a handler, threaded by `State` with no mutation anywhere, or
   * kept as a history — the same three lines of interpreter serve all
   * of them. A `Map` is then one carrier among several, and the demo
   * runs the pure interpretation over two to make the point.
   */
  trait Store[S]:
    def get(s: S, id: Long): Option[String]
    def put(s: S, id: Long, name: String): S

  object Store:
    given Store[Map[Long, String]] with
      def get(s: Map[Long, String], id: Long) = s.get(id)
      def put(s: Map[Long, String], id: Long, name: String) = s + (id -> name)

    /** an association list: a different carrier, the same two laws —
     * what `put` answers, `get` finds */
    given Store[Vector[(Long, String)]] with
      def get(s: Vector[(Long, String)], id: Long) =
        s.collectFirst { case (k, v) if k == id => v }
      def put(s: Vector[(Long, String)], id: Long, name: String) =
        s.filterNot(_._1 == id) :+ (id -> name)

  /**
   * The test world: the same program, no database. Its state is an
   * immutable store in a `var` — the handler answers with a value, so
   * SOMETHING has to hold the successor, and that is the only
   * mutation in it.
   *
   * It does NOT record: recording is `.tracing`, which any handler can
   * wear, including the SQLite one. The operations are already data,
   * so "what did this ask for, and in what order" is a decorator, not
   * a second handler that might drift from the first.
   */
  final class InMemory[S](init: S)(using St: Store[S]) extends Handler[Users]:
    private var s = init
    def state: S = s
    def handle[A](e: Users[A]): A = e match
      case Users.Find(id)       => St.get(s, id)
      case Users.Save(id, name) => s = St.put(s, id, name); ()

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
  type Tracked[S] = State % S + Writer % String

  /**
   * TWO LAYERS, each with one job.
   *
   * `stored` answers the operations, in State and nothing else. It
   * does not log, and it could not: it knows nothing about a Writer
   * being in the row.
   *
   * `!.tracing` records them, and answers nothing: every operation is
   * told to a Writer and then performed exactly as before, so the row
   * keeps `Users` and gains `Writer % String`. It knows nothing about
   * Users beyond `toString`.
   *
   * `tracked` is the two composed, and the order is the meaning:
   * recording happens BEFORE interpretation, so the log holds what
   * the PROGRAM asked, not what the store did about it.
   *
   * `!.interpret` is `translate` with the widening done for it — the
   * target row is bigger than the source's, and F, whatever the
   * caller was already doing, rides through untouched. Inside,
   * `.plus[F]` puts each operation in that row: a for-comprehension
   * fixes its row from the first step, so `State.get` and `State.set`
   * have to arrive already carrying F.
   */
  def stored[A, S : Store as St, F[+_]](prog: A ! (Users + F)): A ! (State % S + F) =
    !.interpret(prog):
      [X] => (e: Users[X]) => e match
        case Users.Find(id) =>
          State.get[S].plus[F].map(St.get(_, id))
        case Users.Save(id, name) =>
          for
            store <- State.get[S].plus[F]
            _     <- State.set(St.put(store, id, name)).plus[F]
          yield ()

  def tracked[A, S : Store, F[+_]](prog: A ! (Users + F)): A ! (Tracked[S] + F) =
    stored[A, S, Writer % String + F](
      !.tracing(prog)([X] => (e: Users[X]) => e.toString))

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

      val mem = InMemory(Map(7L -> "ada"))
      val log = scala.collection.mutable.ListBuffer[Any]()
      println("TEST  " + rename(7L, "grace").runWith(using mem.tracing(log += _)) +
              " / log=" + log.mkString(", ") + " / state=" + mem.state)

      // the id nobody has: the database is untouched, and the trace
      // shows WHY — a find and no save. Note WHICH handler is traced:
      // the SQLite one. Recording is not a test-only trick.
      val missLog = scala.collection.mutable.ListBuffer[Any]()
      val missLive = rename(99L, "hopper").runWith(using live(c).tracing(missLog += _))
      val missTest = rename(99L, "hopper").runWith(using InMemory(Map.empty[Long, String]))
      println(s"MISS  $missLive / row 99 is now ${nameOf(c, 99L)}" +
              s" / both worlds agree: ${missTest == missLive}" +
              s" / log=${missLog.mkString(", ")}")

      println("DIRECT " + initials(7L, 99L).runWith(using live(c)))

      // no mutation anywhere: the store is State, the log is Writer,
      // and the run answers with all three as plain data. Twice, over
      // two carriers — the interpreter is written against `Store`, so
      // neither it nor the program knows which one it got.
      def pureRun[S : Store](init: S): (S, (Seq[String], Option[String])) =
        State.run[S, (Seq[String], Option[String])](init)(
          Writer.run[String, Option[String], State % S](
            tracked[Option[String], S, Pure](rename(7L, "grace"))))
      val (store, (told, answer)) = pureRun(Map(7L -> "ada"))
      println(s"PURE  $answer / log=${told.mkString(", ")} / store=$store")
      val (vecStore, (vecTold, vecAnswer)) = pureRun(Vector(7L -> "ada"))
      println(s"PURE2 $vecAnswer / log=${vecTold.mkString(", ")} / store=$vecStore")
    finally c.close()
