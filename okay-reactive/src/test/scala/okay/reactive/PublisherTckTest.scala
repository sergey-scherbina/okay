package okay.reactive

import java.util.concurrent.Flow
import org.reactivestreams.tck.flow.FlowPublisherVerification
import org.reactivestreams.tck.TestEnvironment
import okay.*
import okay.given

/**
 * The Reactive Streams TCK, run against our publisher.
 *
 * This is not one more suite: it is the only thing that turns "we
 * support reactive streams" from a hope into a claim. The
 * specification is thirty-odd rules about ordering, demand and
 * termination — signals must be serial, `onNext` may not outrun
 * demand, nothing follows a terminal signal, a non-positive request
 * is an error rather than a no-op — and each is a place where a
 * bridge looks right and is wrong. Testing by hand checks the cases
 * we thought of; the TCK checks the ones the specification's authors
 * found the hard way.
 *
 * DRIVEN FROM MUNIT rather than TestNG, and that needs saying. The
 * TCK is a TestNG suite and this build has no TestNG interface — so
 * rather than add a second test runner to the whole repository, the
 * verification's own `@Test` methods are enumerated and invoked, with
 * its `setUp` before each, exactly as TestNG would. The CHECKS are
 * the standard's; only the harness is ours, and a failure names the
 * rule it came from.
 */
class PublisherTckTest extends munit.FunSuite {

  private def verification(): FlowPublisherVerification[Long] =
    new FlowPublisherVerification[Long](TestEnvironment(300L)) {
      given Scheduler = Schedulers.loom

      override def createFlowPublisher(n: Long): Flow.Publisher[Long] =
        Reactive.publisher(Source.range(0L, n))

      /**
       * A publisher ALREADY in an error state — which is what the
       * spec means here, and it took two wrong fixtures to see it.
       *
       * First try: `Source.of(lazyList.map(_ => throw))`. The
       * exception escaped `subscribe` itself, because `Source.of`
       * forces its head, so the TCK reported the bridge as throwing
       * when it was the fixture. Second: `Source.unfold(_ => throw)`,
       * which fails on the PULL — and the pump only pulls once
       * something has been requested, so a subscriber that requests
       * nothing was never told, and the TCK timed out waiting.
       *
       * Terminal signals are not limited by demand. `Reactive.failed`
       * is the publisher that says so.
       */
      override def createFailedFlowPublisher(): Flow.Publisher[Long] =
        Reactive.failed(RuntimeException("boom"))
    }

  private val cases: List[java.lang.reflect.Method] =
    classOf[FlowPublisherVerification[?]].getMethods.toList
      .filter(m => m.getAnnotation(classOf[org.testng.annotations.Test]) != null)
      .filter(_.getParameterCount == 0)
      .sortBy(_.getName)

  test("the TCK finds cases to run at all") {
    // a guard against the whole suite silently doing nothing, which is
    // exactly what happened when this ran under a runner that could
    // not see TestNG annotations: zero tests, zero seconds, green
    assert(cases.length > 30, s"expected the full TCK, found ${cases.length} cases")
  }

  cases.foreach: m =>
    test(s"tck: ${m.getName}") {
      val v = verification()
      v.setUp()
      try m.invoke(v)
      catch
        case e: java.lang.reflect.InvocationTargetException =>
          e.getCause match
            case _: org.testng.SkipException => // the TCK skips what it cannot check here
            case c => throw c
    }
}
