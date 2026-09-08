package okay.demoeff

/**
 * The worked example behind the "define your own effect" note: one
 * program, two interpretations, nothing mocked.
 *
 * `rename` says in its TYPE that it looks up and stores users and
 * does nothing else, and it answers with whatever name was there
 * before — Option, so the id nobody has needs no invented
 * placeholder to keep the signature honest. `live` talks to a real
 * map; `recording` keeps state in memory AND records every
 * operation, which is what makes "what did this ask for, in what
 * order" an assertion rather than a debugging session.
 *
 * Runnable, and its output is the output quoted in the note:
 *   sbt "okayJVM/Test/runMain okay.demoeff.UsersDemo"
 *   PROD  Some(ada) / db={7=grace}
 *   TEST  Some(ada) / log=find(7), save(7,grace) / state=HashMap(7 -> grace)
 *   MISS  None / state=HashMap(99 -> hopper)
 */

import okay.*
import okay.!.*
import okay.given

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

  def live(db: java.util.concurrent.ConcurrentHashMap[Long, String]): Handler[Users] = new:
    def handle[A](e: Users[A]): A = e match
      case Users.Find(id)       => Option(db.get(id))
      case Users.Save(id, name) => db.put(id, name); ()

  def recording(state: scala.collection.mutable.Map[Long, String],
                log: scala.collection.mutable.Buffer[String]): Handler[Users] = new:
    def handle[A](e: Users[A]): A = e match
      case Users.Find(id)       => log += s"find($id)"; state.get(id)
      case Users.Save(id, name) => log += s"save($id,$name)"; state(id) = name; ()

  def main(args: Array[String]): Unit =
    val db = java.util.concurrent.ConcurrentHashMap[Long, String]()
    db.put(7L, "ada")
    println("PROD  " + rename(7L, "grace").runWith(using live(db)) + " / db=" + db)

    val state = scala.collection.mutable.Map(7L -> "ada")
    val log = scala.collection.mutable.ListBuffer[String]()
    val out = rename(7L, "grace").runWith(using recording(state, log))
    println("TEST  " + out + " / log=" + log.mkString(", ") + " / state=" + state)

    val fresh = scala.collection.mutable.Map.empty[Long, String]
    val miss = scala.collection.mutable.ListBuffer[String]()
    println("MISS  " + rename(99L, "hopper").runWith(using recording(fresh, miss)) + " / state=" + fresh)
