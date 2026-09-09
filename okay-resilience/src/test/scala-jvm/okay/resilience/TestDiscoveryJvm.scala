package okay.resilience

import okay.*
import okay.given

/** the resolver: localhost is always there; an unknown name answers empty */
class TestDiscoveryJvm extends munit.FunSuite:

  def run[A](prog: A ! Async): A = Async.run(prog).runWith

  test("dns: localhost resolves to at least one endpoint on the port; an unknown name is empty, not a throw") {
    val d = DiscoveryJvm.dns(8080)
    val local = run(d.resolve("localhost"))
    assert(local.nonEmpty)
    assert(local.forall(_.port == 8080))
    assertEquals(run(d.resolve("no-such-host.invalid")), Vector.empty)
  }

  test("env over sys.env answers empty for a name nobody set") {
    assertEquals(run(DiscoveryJvm.env().resolve("okay-test-unset-service")), Vector.empty)
  }
