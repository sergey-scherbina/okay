package okay

/** jdk-adaptive-scheduler (2026-09-19): the default `given Scheduler`/
 * `given Timer` adapt to whether THIS JVM has virtual threads, with
 * no property required. This box has them (JDK 21+ per .sdkmanrc),
 * so the positive assertions here are what's directly checkable in
 * this process; the JDK17 side is verified separately, out-of-process
 * (specs/jdk-compatibility.md), since there is no way to make a
 * running JVM stop having virtual threads to test the negative branch
 * in-process.
 */
class TestAdaptiveScheduler extends munit.FunSuite:

  test("this JVM reports having virtual threads") {
    assert(Schedulers.hasVirtualThreads, Runtime.version().toString)
  }

  test("auto picks loom when virtual threads are available") {
    assertEquals(Schedulers.auto, Schedulers.loom)
  }

  test("the default given Scheduler is auto's pick, unset") {
    assert(System.getProperty("okay.scheduler") == null)
    val sch = summon[Scheduler]
    assertEquals(sch, Schedulers.auto)
  }
