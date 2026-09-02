package okay.docs

import okay.persist.{MemoryStore, Policy}

/** the own posture: the compacted keyed topic IS a document store */
class TestTopicDocs extends DocsSuite:
  def mkDocs(): Docs[Person] =
    TopicDocs[Person](
      MemoryStore().topic("docs", partitions = 2, policy = Policy(compact = true)),
      Person.indexes)

  test("a cold rebuild refolds the same store: documents, versions, tombstones") {
    val topic = MemoryStore().topic("docs", partitions = 2, policy = Policy(compact = true))
    val warm = TopicDocs[Person](topic, Person.indexes)
    run(warm.put("a", Person("A", "X"))): Unit
    val PutResult.Applied(v2) = run(warm.put("a", Person("A", "Y"))): @unchecked
    run(warm.put("b", Person("B", "X"))): Unit
    run(warm.delete("b")): Unit

    val cold = TopicDocs[Person](topic, Person.indexes)
    assertEquals(run(cold.get("a")), Some(Docs.Versioned(v2, Person("A", "Y"))))
    assertEquals(run(cold.get("b")), None)
  }
