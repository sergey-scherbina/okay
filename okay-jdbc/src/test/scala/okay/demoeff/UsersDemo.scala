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
import okay.!.*
import okay.given
import java.sql.{Connection, DriverManager}

enum Users[+A]:
  case Find(id: Long) extends Users[Option[String]]
  case Save(id: Long, name: String) extends Users[Unit]

object Users:
  given TypeableK[Users] = typeableK(classOf[Users[?]])
  inline def find(id: Long): Option[String] ! Users = effect(Find(id))
  inline def save(id: Long, name: String): Unit ! Users = effect(Save(id, name))

object UsersDemo:

  def rename(id: Long, to: String): Option[String] ! Users =
    for
      old <- Users.find(id)
      _   <- Users.save(id, to)
    yield old

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

  /** the test world: same program, no database, and it records */
  def recording(state: scala.collection.mutable.Map[Long, String],
                log: scala.collection.mutable.Buffer[String]): Handler[Users] = new:
    def handle[A](e: Users[A]): A = e match
      case Users.Find(id)       => log += s"find($id)"; state.get(id)
      case Users.Save(id, name) => log += s"save($id,$name)"; state(id) = name; ()

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
      val log = scala.collection.mutable.ListBuffer[String]()
      println("TEST  " + rename(7L, "grace").runWith(using recording(state, log)) +
              " / log=" + log.mkString(", ") + " / state=" + state)

      println("MISS  " + rename(99L, "hopper").runWith(using live(c)) +
              " / row 99 is now " + nameOf(c, 99L))
    finally c.close()
