package okay2.platform

import okay2.async.Scheduler

/** the default scheduler adapts to whether THIS JVM has virtual threads
 * — the Scala 3 core's TestAdaptiveScheduler; each assertion reads the
 * JVM it is on and checks the branch that JVM takes */
class TestAdaptiveScheduler extends munit.FunSuite {

  /** the flag against the API itself, not the version arithmetic */
  private val canStartVirtual: Boolean =
    try { val _ = Thread.ofVirtual(); true }
    catch { case _: NoSuchMethodError | _: UnsupportedOperationException => false }

  test("hasVirtualThreads says whether Thread.ofVirtual exists on this JVM") {
    assertEquals(Schedulers.hasVirtualThreads, canStartVirtual, Runtime.version().toString)
  }

  test("auto picks loom where there are virtual threads, and a watched own where there are none") {
    if (Schedulers.hasVirtualThreads) assertEquals(Schedulers.auto, Schedulers.loom)
    else Schedulers.auto match {
      case r: Schedulers.Running => r.close()
      case other => fail(s"auto without virtual threads is not an owned scheduler: $other")
    }
  }

  test("the default implicit Scheduler is auto's pick, unset") {
    assert(System.getProperty("okay.scheduler") == null)
    val sch = implicitly[Scheduler]
    if (Schedulers.hasVirtualThreads) assertEquals(sch, Schedulers.loom)
    else assert(sch.isInstanceOf[Schedulers.Running], s"the implicit is not auto's owned pick: $sch")
  }
}
