package okay2.codec

object StubPathModels {
  final case class Task(title: String, done: Boolean, tags: Vector[String])
  object Task { implicit lazy val schema: Schema[Task] = Schema.derived }
  sealed trait Owner
  object Owner {
    final case class Person(name: String) extends Owner
    final case class Team(name: String, size: Int) extends Owner
    implicit lazy val schema: Schema[Owner] = Schema.derived
  }
  final case class Board(name: String, tasks: Vector[Task], owner: Option[Owner])
  object Board { implicit lazy val schema: Schema[Board] = Schema.derived }
}

/** The keys a live subscription may name, as TypeScript types
 * (okay-codec's TestStubsPaths, typescript-types T11) */
class TestStubsPaths extends munit.FunSuite {
  import StubPathModels._

  private def paths: String = Stubs.typescriptPaths(Board.schema, "Board")
  private val anyIndex = "[" + "$" + "{number}]"

  /** the keys the interface declares, with each index as 0 */
  private def keys(src: String): Vector[String] = {
    val body = src.substring(src.indexOf("export interface BoardPaths {"))
    body.linesIterator.toVector.flatMap { l =>
      val t = l.trim
      if (t.startsWith("\"")) Some(t.drop(1).takeWhile(_ != '"'))
      else if (t.startsWith("[k: `")) Some(t.drop(5).takeWhile(_ != '`').replace(anyIndex, "[0]"))
      else None
    }
  }

  test("fields, elements by a template-literal key, and absence as null") {
    val p = paths
    assert(p.contains("  \"name\": string;"), p)
    assert(p.contains("  \"tasks\": Task[];"), p)
    assert(p.contains(s"  [k: `tasks$anyIndex`]: Task | null;"), p)
    assert(p.contains(s"  [k: `tasks$anyIndex.title`]: string | null;"), p)
    assert(p.contains("  \"owner.$case\": Owner | null;"), p)
    assert(p.contains("  \"owner.size\": Int | null;"), p)
    assert(p.contains("  \"\": Board;"), p)
  }

  test("THE LAW: every key the interface declares is one JsonOptic.path accepts") {
    val ks = keys(paths)
    assert(ks.size >= 10, ks)
    assertEquals(ks.filter(k => JsonOptic.path(Board.schema, k).isEmpty), Vector.empty[String])
  }

  test("and a key the schema has no place for is neither declared nor accepted") {
    assert(!keys(paths).contains("tasks[0].nope"))
    assert(JsonOptic.path(Board.schema, "tasks[0].nope").isEmpty)
  }
}
