package okay.persist

import okay.codec.Schema

/** stage 2 of specs/conf.md: audit and rollback with nothing built —
 * the log already was both */
class TestConfigs extends munit.FunSuite {

  final case class Db(url: String, user: String, password: String, poolSize: Int = 8)
  given Schema[Db] = Schema.derived

  def fresh() = Configs(MemoryStore())

  test("latest, rollback-as-a-read, and the audit — three writes, one name") {
    val c = fresh()
    val o1 = c.put("db", Db("jdbc:h2:mem:a", "app", "env:PG1"))
    val o2 = c.put("db", Db("jdbc:h2:mem:a", "app", "env:PG2"))
    val o3 = c.put("db", Db("jdbc:h2:mem:b", "app", "env:PG2", 16))

    assertEquals(c.latest[Db]("db"), Some((o3, Right(Db("jdbc:h2:mem:b", "app", "env:PG2", 16)))))
    // rollback is a read: the state as of o2
    assertEquals(c.at[Db]("db", o2).map(_._2), Some(Right(Db("jdbc:h2:mem:a", "app", "env:PG2"))))
    // the audit: who changed what when is the log itself
    assertEquals(c.history[Db]("db").map(_._1), Vector(o1, o2, o3))
  }

  test("two names on one topic do not bleed — keys filter") {
    val c = fresh()
    c.put("db", Db("u1", "a", "env:X")): Unit
    c.put("cache", Db("u2", "b", "env:Y")): Unit
    assertEquals(c.latest[Db]("db").map(_._2.map(_.url)), Some(Right("u1")))
    assertEquals(c.history[Db]("cache").length, 1)
  }

  test("a damaged stored value is a Left in place; the rest of the history is intact") {
    val c = fresh()
    val good = c.put("db", Db("u", "a", "env:X"))
    c.topic.append("db".getBytes("UTF-8"), "{not json".getBytes("UTF-8"), Ack.Durable): Unit
    val after = c.put("db", Db("u", "a", "env:Y"))
    val h = c.history[Db]("db")
    assertEquals(h.length, 3)
    assertEquals(h(0)._1, good)
    assert(h(1)._2.isLeft)
    assertEquals(h(2)._1, after)
    // latest is the damaged-free newest? No: latest is the NEWEST —
    // here the newest is fine, and the damage sits in the audit
    assertEquals(c.latest[Db]("db").map(_._2.map(_.password)), Some(Right("env:Y")))
  }: Unit

  test("after compact: latest and its offset unchanged, history shortened honestly") {
    val c = fresh()
    c.put("db", Db("u", "a", "env:1")): Unit
    c.put("db", Db("u", "a", "env:2")): Unit
    val last = c.put("db", Db("u", "a", "env:3"))
    val partition = Topic.route("db".getBytes("UTF-8"), c.topic.partitions)
    c.topic.compact(partition)
    assertEquals(c.latest[Db]("db"), Some((last, Right(Db("u", "a", "env:3")))))
    assertEquals(c.history[Db]("db").map(_._1), Vector(last))
  }
}
