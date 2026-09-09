package okay.guice

import okay.*
import okay.given
import com.google.inject.{Guice, Key}
import com.google.inject.name.Names

trait Db { def q: String }
trait Pool { def db: Db }
final class Clock(val now: Long)

object Roles:
  opaque type Primary = Db
  opaque type Replica = Db
  def primary(d: Db): Primary = d
  def replica(d: Db): Replica = d

/** specs/di.md stage 2: the Guice bridge, both ways */
class TestOkayGuice extends munit.FunSuite {

  test("bindings: by name from the plan and by type when unique; the closer releases in reverse") {
    var log = List.empty[String]
    val db = module[Db]({ log ::= "open db"; new Db { val q = "row" } })(_ => log ::= "close db")
    val pool: Db ?=> Module[[X] =>> Pool ?=> X] =
      module[Pool]({ log ::= "open pool"; new Pool { val db = wire[Db] } })(_ => log ::= "close pool")
    val injector = Guice.createInjector(OkayGuice.bindings((db and pool).exports))
    assert(injector.getInstance(classOf[Pool]).db eq injector.getInstance(classOf[Db]))
    assertEquals(injector.getInstance(Key.get(classOf[Db], Names.named("Db"))).q, "row")
    assertEquals(log.reverse, List("open db", "open pool"))
    injector.getInstance(classOf[OkayGuice.ModuleScope]).close()
    assertEquals(log.reverse, List("open db", "open pool", "close pool", "close db"))
  }

  test("two roles over one class are two names and no type binding") {
    import Roles.*
    val m = Module.value[Primary](primary(new Db { val q = "p" })) and Module.value[Replica](replica(new Db { val q = "r" }))
    val injector = Guice.createInjector(OkayGuice.bindings(m.exports))
    assertEquals(injector.getInstance(Key.get(classOf[Db], Names.named("Primary"))).q, "p")
    assertEquals(injector.getInstance(Key.get(classOf[Db], Names.named("Replica"))).q, "r")
    intercept[com.google.inject.ConfigurationException](injector.getInstance(classOf[Db])): Unit
  }

  test("instance[A]: an injector's instance is a module, asked when the scope builds") {
    val injector = Guice.createInjector(new com.google.inject.AbstractModule:
      override def configure(): Unit = bind(classOf[Clock]).toInstance(Clock(7L)))
    val stamp: Clock ?=> Module[[X] =>> String ?=> X] = Module.value[String](s"t@${wire[Clock].now}")
    val got = !.run(Resource.run[String, Nothing]((OkayGuice.instance[Clock](injector) and stamp) { wire[String] }))
    assertEquals(got, "t@7")
  }
}
