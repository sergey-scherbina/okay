package okay.conf

import okay.*
import okay.given
import okay.codec.Schema

/**
 * okay-conf and a module (specs/di.md, stage 1): a config is a
 * `Module.value`, and a secret is resolved INSIDE the module whose
 * acquisition needs it — the value exists only between `Secrets.get`
 * and the constructor argument, never in a stored or logged place.
 */
class TestConfModule extends munit.FunSuite {

  final case class DbConf(url: String, password: Secret) derives Schema
  final class Db(val url: String, val password: String) { var closed = false }

  def run[A](p: A ! Resource): A = !.run(Resource.run[A, Nothing](p))

  test("a config value and a secrets resolver are modules; the connection resolves its secret while acquiring") {
    val conf = Module.value[DbConf](DbConf("jdbc:h2:mem:t", Secret("env:PG_PASSWORD")))
    val secrets = Module.value[Secrets](Secrets.memory(Map("env:PG_PASSWORD" -> "hunter2")))
    val db: (DbConf, Secrets) ?=> Module[[X] =>> Db ?=> X] =
      module[Db](new Db(wire[DbConf].url, wire[Secrets].get(wire[DbConf].password).fold(sys.error, identity)))(_.closed = true)
    val got = run((conf and secrets and db) { (wire[Db].url, wire[Db].password, wire[DbConf].toString) })
    assertEquals(got, ("jdbc:h2:mem:t", "hunter2", "DbConf(jdbc:h2:mem:t,env:PG_PASSWORD)"))
    assertEquals((conf and secrets and db).plan, Vector("DbConf", "Secrets", "Db"))
  }

  test("a secret the resolver cannot answer fails the acquisition and names the reference, not a value") {
    val conf = Module.value[DbConf](DbConf("u", Secret("env:MISSING")))
    val secrets = Module.value[Secrets](Secrets.memory(Map.empty))
    val db: (DbConf, Secrets) ?=> Module[[X] =>> Db ?=> X] =
      module[Db](new Db(wire[DbConf].url, wire[Secrets].get(wire[DbConf].password).fold(sys.error, identity)))(_.closed = true)
    val e = intercept[RuntimeException](run((conf and secrets and db) { wire[Db].url }))
    assert(e.getMessage.contains("env:MISSING"), e.getMessage)
  }
}
