package okay.spring

import okay.*
import okay.given
import org.springframework.context.support.GenericApplicationContext
import org.springframework.core.ReactiveAdapterRegistry
import org.springframework.boot.autoconfigure.AutoConfigurations
import org.springframework.boot.test.context.runner.ApplicationContextRunner
import reactor.core.publisher.Mono

trait Db { def q: String }
trait Pool { def db: Db }
final class Clock(val now: Long)

/** specs/di.md stage 2: the Spring bridge, both ways, and the adapter */
class TestOkaySpring extends munit.FunSuite {

  test("register: one singleton per installed capability, the dependent one built over the earlier, closed with the context in reverse") {
    var log = List.empty[String]
    val db = module[Db]({ log ::= "open db"; new Db { val q = "row" } })(_ => log ::= "close db")
    val pool: Db ?=> Module[[X] =>> Pool ?=> X] =
      module[Pool]({ log ::= "open pool"; new Pool { val db = wire[Db] } })(_ => log ::= "close pool")
    val ctx = GenericApplicationContext()
    OkaySpring.register(ctx, (db and pool).exports)
    ctx.refresh()
    val p = ctx.getBean(classOf[Pool])
    assert(p.db eq ctx.getBean(classOf[Db]))
    assertEquals(ctx.getBean("Db", classOf[Db]).q, "row")
    assertEquals(log.reverse, List("open db", "open pool"))
    ctx.close()
    assertEquals(log.reverse, List("open db", "open pool", "close pool", "close db"))
  }

  test("bean[A]: a Spring bean is a module, looked up when the scope builds") {
    val ctx = GenericApplicationContext()
    ctx.registerBean(classOf[Clock], (() => Clock(7L)): java.util.function.Supplier[Clock])
    ctx.refresh()
    val stamp: Clock ?=> Module[[X] =>> String ?=> X] = Module.value[String](s"t@${wire[Clock].now}")
    val got = !.run(Resource.run[String, Nothing]((OkaySpring.bean[Clock](ctx) and stamp) { wire[String] }))
    assertEquals(got, "t@7")
    ctx.close()
  }

  test("adapter: a program becomes a publisher, a publisher becomes a program") {
    val registry = ReactiveAdapterRegistry()
    OkayReactive.registerAdapter(registry)
    val adapter = registry.getAdapter(OkayReactive.programClass)
    assert(adapter != null)
    val prog: Int ! Async = async(41).map(_ + 1)
    assertEquals(Mono.from(adapter.toPublisher[Int](prog)).block(), 42)
    assertEquals(OkayReactive.fromPublisher(Mono.just("y")).runWith, "y")
    adapter.fromPublisher(Mono.just("x")) match
      case p: Free[?, ?] => assertEquals(p.getClass.getSimpleName.isEmpty, false)
      case other => fail(s"not a program: $other")
  }

  test("the Boot auto-configuration registers the adapter on the shared registry") {
    ApplicationContextRunner()
      .withConfiguration(AutoConfigurations.of(classOf[OkayAutoConfiguration]))
      .run { ctx =>
        assert(ctx.getBean(classOf[OkayReactiveAdapter]) != null)
        assert(ReactiveAdapterRegistry.getSharedInstance.getAdapter(OkayReactive.programClass) != null)
      }
  }
}
