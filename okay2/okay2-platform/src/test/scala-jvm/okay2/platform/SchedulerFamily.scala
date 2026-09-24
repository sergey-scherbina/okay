package okay2.platform

import okay2.async.Scheduler

/**
 * The scheduler family every law is run against, and the `each` that
 * runs one law against all of them — the Scala 3 core's SchedulerFamily.
 * A trait rather than a list inside one suite, so a scheduler added here
 * reaches every suite that runs the family.
 */
trait SchedulerFamily extends munit.FunSuite {

  private val owned: List[(String, Schedulers.Running)] = List(
    "own" -> Schedulers.own.workers(4).build,
    "own.forShortTasks" -> Schedulers.own.workers(4).forShortTasks.build,
    "own.forLongTasks" -> Schedulers.own.workers(4).forLongTasks.build,
    "adaptive" -> Schedulers.adaptive.workers(2).build)

  protected val members: List[(String, Scheduler)] =
    List("loom" -> Schedulers.loom, "drive" -> Schedulers.drive()) ++ owned

  override def afterAll(): Unit = owned.foreach(_._2.close())

  /** one law, run against every member, each as its own test; `loom` is
   * SKIPPED on a JVM without virtual threads (its first fork would be a
   * NoSuchMethodError, which munit treats as fatal for the rest) */
  protected def each(name: String)(law: Scheduler => Unit): Unit =
    members.foreach { case (member, sch) =>
      test(s"$name — $member") {
        if (member == "loom") assume(Schedulers.hasVirtualThreads, "loom: this JVM has no virtual threads")
        law(sch)
      }
    }
}
