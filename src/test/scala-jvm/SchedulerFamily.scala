package okay

/**
 * The scheduler family every law in specs/schedulers.md is run
 * against, and the `each` that runs one law against all of them.
 *
 * It is a trait rather than a private list inside `TestSchedulerLaws`
 * because core-modules stage 1 moved two of those laws to okay-stream
 * — the two whose blocking device is a `Channel`, which is no longer
 * in the core. A copied member list would have been the cheaper edit
 * and the wrong one: a scheduler added here would then reach one
 * suite and silently not the other.
 */
trait SchedulerFamily extends munit.FunSuite:

  private val owned: List[(String, Schedulers.Running)] = List(
    "own" -> Schedulers.own.workers(4).build,
    "own.forShortTasks" -> Schedulers.own.workers(4).forShortTasks.build,
    "own.forLongTasks" -> Schedulers.own.workers(4).forLongTasks.build,
    "adaptive" -> Schedulers.adaptive.workers(2).build)

  protected val members: List[(String, Scheduler)] =
    List("loom" -> Schedulers.loom, "drive" -> Schedulers.drive()) ++ owned

  override def afterAll(): Unit = owned.foreach(_._2.close())

  /** one law, run against every member, each as its own test */
  protected def each(name: String)(law: Scheduler => Unit): Unit =
    members.foreach { case (member, sch) => test(s"$name — $member") { law(sch) } }
