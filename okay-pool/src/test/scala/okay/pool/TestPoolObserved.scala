package okay.pool

import okay.given
import okay.codec.Json
import okay.http.{Body, Method, Request}
import okay.resilience.Discovery
import java.nio.charset.StandardCharsets.UTF_8

/**
 * THE POOL SAYS WHAT ITS RUNS DID (specs/dataflow.md, stage 15): a run
 * coordinated here adds to `/metrics` and leaves its trace, as OTLP
 * JSON, at `/pool/runs/{id}/trace`.
 */
class TestPoolObserved extends munit.FunSuite {
  CountJobs.install()

  def body(r: okay.http.Response): String = new String(okay.http.Http.bytes(r).runWith.toArray, UTF_8)

  test("a run's metrics reach /metrics and its trace is OTLP JSON") {
    val store = SharedStore()
    val router = Routes.router(PoolConf(), Discovery.static(Map.empty), store(_), () => true)
    val submit = Json.print(Json.JObj(Vector("params" -> Json.JNum(10), "journal" -> Json.JStr("observed-run"))))
    val posted = router.routes(Request(Method.Post, s"/pool/jobs/${CountJob.name}", Nil, Body.Text(submit))).runWith
    assertEquals(posted.status, 202)
    var status = ""
    var tries = 0
    while !status.contains("\"Done\"") && tries < 500 do
      Thread.sleep(10)
      status = body(router.routes(Request.get("/pool/runs/observed-run")).runWith)
      tries += 1
    assert(status.contains("\"Done\""), s"never finished: $status")

    val metrics = body(router.routes(Request.get("/metrics")).runWith)
    assert(metrics.contains("okay_pool_queued"), metrics)
    assert(metrics.contains(s"""okay_job_runs_total{job="${CountJob.name}",outcome="ok"}"""), metrics)
    assert(metrics.contains("okay_job_epoch{"), metrics)

    val trace = router.routes(Request.get("/pool/runs/observed-run/trace")).runWith
    assertEquals(trace.status, 200)
    val otlp = body(trace)
    assert(otlp.contains("resourceSpans") && otlp.contains(s"job ${CountJob.name}"), otlp)
    assert(otlp.contains("\"name\":\"epoch 1\""), otlp)

    val none = router.routes(Request.get("/pool/runs/nobody/trace")).runWith
    assertEquals(none.status, 404)
  }
}
