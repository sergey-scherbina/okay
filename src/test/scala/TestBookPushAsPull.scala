package okay

import okay.Direct.*
import scala.language.implicitConversions

/**
 * THE BOOK'S CHAPTER 6, COMPILED (docs/continuations/06-push-as-pull.md).
 */
class TestBookPushAsPull extends munit.FunSuite {

  type Row = Delim + Pure

  // ---- the domain: a directory tree, walked recursively

  enum Entry:
    case File(name: String, bytes: Int)
    case Dir(name: String, entries: List[Entry])

  val tree = Entry.Dir("/", List(
    Entry.File("a.txt", 10),
    Entry.Dir("src", List(
      Entry.File("Main.scala", 200),
      Entry.File("Util.scala", 50))),
    Entry.File("b.log", 4000)))

  // ---- the producer: it emits, and knows nothing about the consumer

  def walk(e: Entry, prefix: String = "")(using Delim.Emitting[String]): Unit ! Row =
    direct:
      e match
        case Entry.File(n, _) => !Delim.emit(s"$prefix$n")
        case Entry.Dir(n, es) =>
          val here = if n == "/" then "/" else s"$prefix$n/"
          for child <- es do !walk(child, here)

  test("the producer stays a recursive walk; the caller gets a list") {
    assertEquals(!.run(Delim.collect[String, Pure](walk(tree))),
      List("/a.txt", "/src/Main.scala", "/src/Util.scala", "/b.log"))
  }

  test("a producer that emits nothing is not a special case") {
    assertEquals(!.run(Delim.collect[String, Pure](direct(()))), List.empty[String])
  }

  // ---- the callback version, for the comparison the chapter makes

  def walkCallback(e: Entry, prefix: String = "")(onFile: String => Unit): Unit =
    e match
      case Entry.File(n, _) => onFile(s"$prefix$n")
      case Entry.Dir(n, es) =>
        val here = if n == "/" then "/" else s"$prefix$n/"
        es.foreach(walkCallback(_, here)(onFile))

  test("the callback version agrees — which is what makes the comparison fair") {
    val buf = List.newBuilder[String]
    walkCallback(tree)(buf += _)
    assertEquals(buf.result(), !.run(Delim.collect[String, Pure](walk(tree))))
  }

  // ---- the limit this chapter is honest about: collect runs it ALL

  test("collect runs the producer to the end, and the counter proves it") {
    var emitted = 0
    def counting(e: Entry, prefix: String = "")(using Delim.Emitting[String]): Unit ! Row =
      direct:
        e match
          case Entry.File(n, _) =>
            emitted += 1
            !Delim.emit(s"$prefix$n")
          case Entry.Dir(n, es) =>
            val here = if n == "/" then "/" else s"$prefix$n/"
            for child <- es do !counting(child, here)
    val all = !.run(Delim.collect[String, Pure](counting(tree)))
    assertEquals(all.size, 4)
    assertEquals(emitted, 4, "collect did not run the whole producer")
  }
}
