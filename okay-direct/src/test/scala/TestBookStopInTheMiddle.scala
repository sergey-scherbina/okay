package okay

import okay.Direct.*
import scala.language.implicitConversions

/**
 * THE BOOK'S CHAPTER 7, COMPILED (docs/continuations/07-stop-in-the-middle.md).
 *
 * The same directory walk as chapter 6, pulled one file at a time --
 * and the same booking dialogue the library's own pattern suite uses,
 * because a paused program being a VALUE is the claim the chapter
 * rests on and it deserves more than one witness.
 */
class TestBookStopInTheMiddle extends munit.FunSuite {

  type Row = Delim + Pure

  enum Entry:
    case File(name: String, bytes: Int)
    case Dir(name: String, entries: List[Entry])

  val tree = Entry.Dir("/", List(
    Entry.File("a.txt", 10),
    Entry.Dir("src", List(
      Entry.File("Main.scala", 200),
      Entry.File("Util.scala", 50))),
    Entry.File("b.log", 4000)))

  // ---- the SAME producer as chapter 6, but it PAUSES at each file
  //      instead of emitting. It asks "here is a file" and waits.

  def walk(e: Entry, prefix: String = "")
          (using Delim.Asking[String, Unit, Unit, Row]): Unit ! Row =
    direct:
      e match
        case Entry.File(n, _) => !Delim.pause(s"$prefix$n")
        case Entry.Dir(n, es) =>
          val here = if n == "/" then "/" else s"$prefix$n/"
          for child <- es do !walk(child, here)

  /** take the first n, and do NOT run the rest */
  def take(n: Int): (List[String], Int) =
    var got = List.empty[String]
    var pulls = 0
    var p = !.run(Delim.resumable[String, Unit, Unit, Pure](walk(tree)))
    var going = true
    while going do
      p match
        case Delim.Paused.Ask(q, _, _) if got.size < n =>
          got = got :+ q
          pulls += 1
          p = !.run(Delim.answer(p, Nil)(()))._1
        case _ => going = false
    (got, pulls)

  test("THE POINT: the first two files, and the rest is never walked") {
    val (files, pulls) = take(2)
    assertEquals(files, List("/a.txt", "/src/Main.scala"))
    assertEquals(pulls, 2, "it kept walking after it had enough")
  }

  test("taking more than there is stops at the end, not at the number") {
    val (files, _) = take(99)
    assertEquals(files, List("/a.txt", "/src/Main.scala", "/src/Util.scala", "/b.log"))
  }

  // ---- two producers read IN STEP, which no callback can do

  val other = Entry.Dir("/", List(
    Entry.File("x.md", 1), Entry.File("y.md", 2), Entry.File("z.md", 3)))

  test("two walks, interleaved, with no thread anywhere") {
    var a = !.run(Delim.resumable[String, Unit, Unit, Pure](walk(tree)))
    var b = !.run(Delim.resumable[String, Unit, Unit, Pure](walk(other)))
    var pairs = List.empty[(String, String)]
    var going = true
    while going do
      (a, b) match
        case (Delim.Paused.Ask(qa, _, _), Delim.Paused.Ask(qb, _, _)) =>
          pairs = pairs :+ (qa, qb)
          a = !.run(Delim.answer(a, Nil)(()))._1
          b = !.run(Delim.answer(b, Nil)(()))._1
        case _ => going = false
    assertEquals(pairs, List(
      ("/a.txt", "/x.md"), ("/src/Main.scala", "/y.md"), ("/src/Util.scala", "/z.md")))
  }

  // ---- a paused program is a VALUE: resuming does not consume it

  def booking(using Delim.Asking[String, String, String, Row]): String ! Row = direct:
    val city = !Delim.pause("Which city?")
    val nights = !Delim.pause(s"How many nights in $city?")
    s"$city/$nights"

  test("one pause, two futures — the same value answered twice, differently") {
    val start = !.run(Delim.resumable[String, String, String, Pure](booking))
    def answering(as: List[String]): String => String ! Pure =
      var left = as
      _ => { val a = left.head; left = left.tail; okay.pure(a) }

    assertEquals(!.run(Delim.drive(start)(answering(List("Kyiv", "3")))), "Kyiv/3")
    // the SAME start, answered again: it was not consumed
    assertEquals(!.run(Delim.drive(start)(answering(List("Lviv", "2")))), "Lviv/2")
  }

  test("a program that never pauses is Done before anybody drives it") {
    val p = !.run(Delim.resumable[String, String, Int, Pure](direct(41 + 1)))
    assert(p.isInstanceOf[Delim.Paused.Done[?, ?, ?, ?]], s"expected Done, got $p")
    assertEquals(!.run(Delim.drive(p)(_ => okay.pure("unused"))), 42)
  }
}
