package okay

/** jdk-adaptive-scheduler (2026-09-19): the default `given Scheduler`/
 * `given Timer` adapt to whether THIS JVM has virtual threads, with
 * no property required. There is no way to make a running JVM stop
 * having virtual threads, so each assertion here reads the JVM it is
 * on and checks the branch that JVM takes: `sbt verifyJdk17` runs
 * this same suite on a real 17 (specs/jdk-compatibility.md), where
 * the other branch is the one under test (jdk17-core-loom-tests).
 */
class TestAdaptiveScheduler extends munit.FunSuite:

  /** the flag against the API itself, not against the version arithmetic that sets it */
  private val canStartVirtual: Boolean =
    try { Thread.ofVirtual(); true }
    catch case _: NoSuchMethodError | _: UnsupportedOperationException => false

  test("hasVirtualThreads says whether Thread.ofVirtual exists on this JVM") {
    assertEquals(Schedulers.hasVirtualThreads, canStartVirtual, Runtime.version().toString)
  }

  test("auto picks loom where there are virtual threads, and a watched own where there are none") {
    if Schedulers.hasVirtualThreads then assertEquals(Schedulers.auto, Schedulers.loom)
    else Schedulers.auto match
      case r: Schedulers.Running => r.close()   // own-lost-wakeup: platform, an owned scheduler with the stuck-check on
      case other => fail(s"auto without virtual threads is not an owned scheduler: $other")
  }

  test("the default given Scheduler is auto's pick, unset") {
    assert(System.getProperty("okay.scheduler") == null)
    val sch = summon[Scheduler]
    if Schedulers.hasVirtualThreads then assertEquals(sch, Schedulers.loom)
    else assert(sch.isInstanceOf[Schedulers.Running], s"the given is not auto's owned pick: $sch")
  }
