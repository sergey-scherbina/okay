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

  /** replace a name, ANSWERING the one that was there — `None` if
   * this handler had to invent the row. `rename` does not use that
   * answer (it asks first, which is what the pattern below is for),
   * but an operation with something to say costs nothing and saves
   * every interpreter a `.map(_ => ())` (specs/writer-covariance.md,
   * signature-covariance). */
  case Save(id: Long, name: String) extends Users[Option[String]]

object Users:
  inline def find(id: Long): Option[String] ! Users = effect(Find(id))
  inline def save(id: Long, name: String): Option[String] ! Users = effect(Save(id, name))

object UsersDemo:

  /**
   * Renaming somebody who is not there is not a rename, and a
   * for-comprehension SEQUENCES rather than branches — so `find` then
   * `save` would reach the handler's upsert and CREATE the user.
   *
   * The fix is the pattern, plus a row that says this program MAY
   * STOP. `case Some(old) <-` desugars to `withFilter`, `withFilter`
   * needs somewhere for the dropped step to go, and `Abort` — failure
   * carrying no information, which is all a missing row has to say —
   * is that somewhere (Fail.scala). `save` cannot run for a missing id
   * because it is NOT REACHABLE, not because a branch remembered to
   * skip it, and the SQLite handler below would happily have created
   * the row if it had been asked.
   *
   * Two operations, deliberately. Modelling `Save` to answer the name
   * it replaced would collapse this to `Users.save(id, to)` and remove
   * the question — which is the better design and a worse
   * demonstration, so it is worth knowing and not what this file
   * shows.
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


  private def selectName(c: Connection, id: Long): Option[String] =
    val ps = c.prepareStatement("select name from users where id = ?")
    try
      ps.setLong(1, id)
      val rs = ps.executeQuery()
      if rs.next() then Some(rs.getString(1)) else None
    finally ps.close()

  /** the real world: a SQLite file */
  def live(c: Connection): Handler[Users] = new:
    def handle[A](e: Users[A]): A = e match
      case Users.Find(id) => selectName(c, id)
      case Users.Save(id, name) =>
        // an UPSERT: asked about an id nobody has, this handler would
        // create the row. The program never asks, and that is the
        // point — the guarantee is in the program's type, not in the
        // handler's good manners.
        val was = selectName(c, id)
        val ps = c.prepareStatement(
          "insert into users(id, name) values (?, ?) " +
          "on conflict(id) do update set name = excluded.name")
        try { ps.setLong(1, id); ps.setString(2, name); ps.executeUpdate(): Unit }
        finally ps.close()
        was

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
    def get(id: Long): S => Option[String]
    def put(id: Long, name: String): S => S

    /**
     * REPLACE, answering what was there — the two above in the order
     * that makes the answer true, said once here instead of at every
     * call site.
     *
     * Writing `(get(id)(s), put(id, name)(s))` at a use site is
     * correct only because the tuple evaluates left to right and
     * because `put` leaves `s` alone. Neither is enforced by anything:
     * the immutability is a LAW of this class, and a law is documented
     * and tested, not checked by the compiler. So the sequencing lives
     * in the contract, where the instance author owns it — and a
     * carrier that can do the swap in one step (a persistent map with
     * a `getAndUpdate`, a cell, a database row) overrides this and
     * does not depend on the law at all.
     */
    def replace(id: Long, name: String): S => (Option[String], S) =
      s =>
        val was = get(id)(s)
        (was, put(id, name)(s))

  object Store:
    given Store[Map[Long, String]] with
      def get(id: Long) = _.get(id)
      def put(id: Long, name: String) = _ + (id -> name)

    /** an association list: a different carrier, the same two laws —
     * what `put` answers, `get` finds */
    given Store[Vector[(Long, String)]] with
      def get(id: Long) = _.collectFirst { case (k, v) if k == id => v }
      def put(id: Long, name: String) = s => s.filterNot(_._1 == id) :+ (id -> name)

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
      case Users.Find(id)       => St.get(id)(s)
      case Users.Save(id, name) =>
        val (was, next) = St.replace(id, name)(s)
        s = next
        was

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
   * `.plus[F]` puts each step in that row: the constructors build at
   * `State % S`, and the branch has to answer in `State % S + F`.
   */
  def stored[A, S : Store as S, F[+_]](prog: A ! (Users + F)): A ! (State % S + F) =
    !.interpret(prog):
      [X] => (e: Users[X]) => e match
        case Users.Find(id) =>
          State.get[S].plus[F].map(S.get(id))
        case Users.Save(id, name) =>
          // NOT `modify`: `Save` answers the name that WAS there, and
          // the write destroys it. `update` is the transition that
          // answers something the write is about to destroy, and
          // `Store.replace` is the read-then-write in the order that
          // makes that answer true.
          State.update[S, X](S.replace(id, name)).plus[F]

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
      val st2 = c.createStatement()
      st2.execute("create table users(id integer primary key not null, name text not null)")
      st2.execute("insert into users values (7, 'ada')")

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
