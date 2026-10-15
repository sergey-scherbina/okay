package okay.graphql

import okay.AsyncCont
import okay.codec.Json
import okay.one.Cont.pure
import scala.collection.mutable

class TestGraphQLCache extends okay.testkit.Munit.Diagnosed:
  private given scala.concurrent.ExecutionContext = munitExecutionContext

  /** a source of key lengths recording its calls; "missing" it leaves out */
  private def counted(name: String, cached: Boolean = true) =
    val calls = mutable.ArrayBuffer.empty[Seq[String]]
    val src = Source[String, Int](name, cached) { keys =>
      calls += keys
      keys.filter(_ != "missing").map(k => k -> k.length).toMap
    }
    (src, calls)

  /** `now` asks `key` in round 1; `later` asks `other` first, then `key`: round 2 */
  private def schema(src: DataSource[String, Int], other: DataSource[String, Int]) = Schema(ObjectType[Unit]("Query")(List(
    Field.fetch("now", Nullable(Scalar.int), Arg("key", InScalar.string))((_, a) =>
      Fetch.get(src, a(Arg("key", InScalar.string))).map(Some(_))),
    Field.fetch("later", Nullable(Scalar.int), Arg("key", InScalar.string))((_, a) =>
      Fetch.get(other, "x").flatMap(_ => Fetch.get(src, a(Arg("key", InScalar.string)))).map(Some(_))))))

  private def run(s: Schema, q: String): Json =
    val out = Executor.execute(s, q)
    note(s"$q => ${Json.print(out)}")
    out

  test("a key in two rounds: one call; the cache is the execution's, not shared") {
    val (src, calls) = counted("lengths")
    val (other, _) = counted("other")
    val s = schema(src, other)
    assertEquals(run(s, """{ now(key: "abc") later(key: "abc") }"""), Json.parse("""{"data":{"now":3,"later":3}}"""))
    assertEquals(calls.toList, List(Seq("abc")))
    run(s, """{ now(key: "abc") }"""): Unit
    assertEquals(calls.size, 2)
  }

  test("an error is cached as a value is") {
    val (src, calls) = counted("lengths")
    val (other, _) = counted("other")
    val out = run(schema(src, other), """{ now(key: "missing") later(key: "missing") }""")
    assertEquals(Json.print(out),
      """{"data":{"now":null,"later":null},"errors":[{"message":"lengths has no value for missing","locations":[{"line":1,"column":3}],"path":["now"]},""" +
      """{"message":"lengths has no value for missing","locations":[{"line":1,"column":23}],"path":["later"]}]}""")
    assertEquals(calls.toList, List(Seq("missing")))
  }

  test("a source that opts out is asked each round") {
    val (src, calls) = counted("clock", cached = false)
    val (other, _) = counted("other")
    run(schema(src, other), """{ now(key: "abc") later(key: "abc") }"""): Unit
    assertEquals(calls.toList, List(Seq("abc"), Seq("abc")))
  }

  test("a mutation's fields see fresh data: the cache forgotten between them") {
    var stock = 5
    val calls = mutable.ArrayBuffer.empty[Seq[String]]
    val stockOf = Source[String, Int]("stock") { keys => calls += keys; keys.map(_ -> stock).toMap }
    val s = Schema(ObjectType[Unit]("Query")(List(Field("ok", Scalar.boolean)(_ => true))),
      mutation = Some(ObjectType[Unit]("Mutation")(List(
        Field.fetch("buy", Scalar.int)((_, _) => Fetch.get(stockOf, "book").map { n => stock = n - 1; stock }),
        Field.fetch("left", Scalar.int)((_, _) => Fetch.get(stockOf, "book"))))))
    assertEquals(run(s, "mutation { a: buy b: buy c: left }"), Json.parse("""{"data":{"a":4,"b":3,"c":3}}"""))
    assertEquals(calls.size, 3)
  }

  test("async sources cached the same") {
    val calls = mutable.ArrayBuffer.empty[Seq[String]]
    val src = AsyncSource[String, Int]("lengths") { keys => calls += keys; pure(keys.map(k => k -> k.length).toMap) }
    val other = AsyncSource[String, Int]("other")(keys => pure(keys.map(_ -> 0).toMap))
    AsyncCont.runAsync(Executor.executeAsync(schema(src, other), """{ now(key: "ab") later(key: "ab") }""")).map { out =>
      assertEquals(out, Json.parse("""{"data":{"now":2,"later":2}}"""))
      assertEquals(calls.toList, List(Seq("ab")))
    }
  }
