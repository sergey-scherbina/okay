package okay.codec

object TestStubsPaths:
  final case class Task(title: String, done: Boolean, tags: Vector[String]) derives Schema
  enum Owner derives Schema:
    case Person(name: String)
    case Team(name: String, size: Int)
  final case class Board(name: String, tasks: Vector[Task], owner: Option[Owner]) derives Schema

/** typescript-types T11: the keys a live subscription may name, as TypeScript types */
class TestStubsPaths extends munit.FunSuite {
  import TestStubsPaths.*

  // a DEF: an object val summoning a nested case class's Schema stalled once (ts-types-check)
  private def paths: String = Stubs.typescriptPaths(summon[Schema[Board]], "Board")
  private val anyIndex = "[" + "$" + "{number}]"

  /** the keys the interface declares, with each index as 0 */
  private def keys(src: String): Vector[String] =
    val body = src.substring(src.indexOf("export interface BoardPaths {"))
    body.linesIterator.toVector.flatMap { l =>
      val t = l.trim
      if t.startsWith("\"") then Some(t.drop(1).takeWhile(_ != '"'))
      else if t.startsWith("[k: `") then Some(t.drop(5).takeWhile(_ != '`').replace(anyIndex, "[0]"))
      else None
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
    val refused = ks.filter(k => JsonOptic.path(summon[Schema[Board]], k).isEmpty)
    assertEquals(refused, Vector.empty)
  }

  test("and a key the schema has no place for is neither declared nor accepted") {
    assert(!keys(paths).contains("tasks[0].nope"))
    assert(JsonOptic.path(summon[Schema[Board]], "tasks[0].nope").isEmpty)
  }
}
