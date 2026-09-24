package okay.pool

import okay.codec.Json

class TestPoolConf extends munit.FunSuite {

  test("defaults with no file and no environment") {
    val conf = PoolConf.load(env = _ => None).toOption.get
    assertEquals(conf.port, 7100)
    assertEquals(conf.httpPort, 7101)
    assertEquals(conf.store, "")
    assertEquals(conf.tolerance, 3)
  }

  test("the environment overrides a default, by the derived name") {
    val env = Map("OKAYPOOL_PORT" -> "9000", "OKAYPOOL_SERVICE" -> "pool", "OKAYPOOL_BUILD" -> "abc123")
    val conf = PoolConf.load(env = env.get).toOption.get
    assertEquals(conf.port, 9000)
    assertEquals(conf.service, "pool")
    assertEquals(conf.build, "abc123")
    // untouched fields keep their default
    assertEquals(conf.httpPort, 7101)
  }

  test("OKAYPOOL_CONF names a file that cannot be read: a named refusal") {
    val env = Map("OKAYPOOL_CONF" -> "/no/such/file")
    val out = PoolConf.load(env = env.get, slurp = _ => Left("nope"))
    assert(out.isLeft)
    assert(out.left.toOption.get.contains("OKAYPOOL_CONF"))
  }

  test("a file sets a field, the environment still overrides it") {
    val fileText = """{"port": 8000}"""
    val env = Map("OKAYPOOL_CONF" -> "conf.json", "OKAYPOOL_PORT" -> "9500")
    val conf = PoolConf.load(env = env.get, slurp = _ => Right(fileText)).toOption.get
    assertEquals(conf.port, 9500)
  }
}

class TestRunMeta extends munit.FunSuite {

  test("a run record round-trips through its bytes") {
    val m = RunMeta("some.job", Json.JObj(Vector("n" -> Json.JNum(42))), 4, 100)
    val back = RunMeta.decode(RunMeta.encode(m))
    assertEquals(back, Right(m))
  }

  test("a record missing a required field is a named refusal") {
    val bytes = Json.print(Json.JObj(Vector("job" -> Json.JStr("x")))).getBytes(java.nio.charset.StandardCharsets.UTF_8)
    assert(RunMeta.decode(bytes).isLeft)
  }

  test("a record that is not an object is a named refusal") {
    val bytes = "\"just a string\"".getBytes(java.nio.charset.StandardCharsets.UTF_8)
    assert(RunMeta.decode(bytes).isLeft)
  }

  test("params defaults to an empty object when absent") {
    val bytes = Json.print(Json.JObj(Vector(
      "job" -> Json.JStr("x"), "parts" -> Json.JNum(1), "take" -> Json.JNum(0)))).getBytes(java.nio.charset.StandardCharsets.UTF_8)
    val m = RunMeta.decode(bytes).toOption.get
    assertEquals(m.params, Json.JObj(Vector.empty))
  }
}
