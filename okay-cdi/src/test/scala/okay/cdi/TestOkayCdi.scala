package okay.cdi

import okay.*
import okay.given
import jakarta.enterprise.inject.literal.NamedLiteral
import jakarta.enterprise.inject.se.SeContainerInitializer

trait Db { def q: String }
trait Pool { def db: Db }
final class Clock(val now: Long)

object Roles:
  opaque type Primary = Db
  opaque type Replica = Db
  def primary(d: Db): Primary = d
  def replica(d: Db): Replica = d

/** specs/di.md stage 2: the CDI bridge on Weld SE, both ways */
class TestOkayCdi extends munit.FunSuite {

  private def container(ext: jakarta.enterprise.inject.spi.Extension) =
    SeContainerInitializer.newInstance().disableDiscovery().addExtensions(ext).initialize()

  test("extension: one bean per installed capability, named by the plan, the dependent one over the earlier; shutdown releases in reverse") {
    var log = List.empty[String]
    val db = module[Db]({ log ::= "open db"; new Db { val q = "row" } })(_ => log ::= "close db")
    val pool: Db ?=> Module[[X] =>> Pool ?=> X] =
      module[Pool]({ log ::= "open pool"; new Pool { val db = wire[Db] } })(_ => log ::= "close pool")
    val c = container(OkayCdi.extension((db and pool).exports))
    assert(c.select(classOf[Pool]).get().db eq c.select(classOf[Db]).get())
    assertEquals(c.select(classOf[Db], NamedLiteral.of("Db")).get().q, "row")
    assertEquals(log.reverse, List("open db", "open pool"))
    c.close()
    assertEquals(log.reverse, List("open db", "open pool", "close pool", "close db"))
  }

  test("two roles over one class are two names, and ambiguous by type") {
    import Roles.*
    val m = Module.value[Primary](primary(new Db { val q = "p" })) and Module.value[Replica](replica(new Db { val q = "r" }))
    val c = container(OkayCdi.extension(m.exports))
    assertEquals(c.select(classOf[Db], NamedLiteral.of("Primary")).get().q, "p")
    assertEquals(c.select(classOf[Db], NamedLiteral.of("Replica")).get().q, "r")
    assert(c.select(classOf[Db]).isAmbiguous)
    c.close()
  }

  test("instance[A]: a container's instance is a module, selected when the scope builds") {
    val clock = Module.value[Clock](Clock(7L))
    val c = container(OkayCdi.extension(clock.exports))
    val stamp: Clock ?=> Module[[X] =>> String ?=> X] = Module.value[String](s"t@${wire[Clock].now}")
    val got = !.run(Resource.run[String, Nothing]((OkayCdi.instance[Clock](c) and stamp) { wire[String] }))
    assertEquals(got, "t@7")
    c.close()
  }
}
