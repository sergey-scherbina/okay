package okay.kernel

import okay.Resource
import scala.collection.mutable

/** the plan and the start, against the behaviour list of specs/kernel.md */
class TestKernel extends munit.FunSuite:

  trait Clock { def now: Long }
  trait Source { def name: String }
  trait Sink { def put(s: String): Unit }

  val clock: Port[Clock] = Port.one("clock", Version("1.2"))
  val sources: Port[Source] = Port.many("source", Version("2.0"),
    Law.holds[Source]("a source has a name")(_.name.nonEmpty))
  val sink: Port[Sink] = Port.one("sink", Version("1.0"))

  /** a plugin as data, for the tests */
  final case class P(id: String, needs: Vector[Need] = Vector.empty,
                     provides: Vector[Provision[?]] = Vector.empty,
                     version: Version = Version("1.0"),
                     override val kernel: Range = Range.Caret(Kernel.api)) extends Plugin

  def clockAt(t: Long, built: String = "1.2") = Provision.value(clock, built)(_ => new Clock { def now = t })
  def source(n: String, built: String = "2.0") = Provision.value(sources, built)(_ => new Source { def name = n })

  def problems(ps: Plugin*)(using loc: munit.Location): Vector[Problem] =
    Kernel.plan(ps).fold(identity, p => fail(s"expected problems, got a plan: ${p.describe}"))

  test("a plan answers every problem at once, not the first") {
    val ps = problems(
      P("a", needs = Vector(Need.of(clock, "^1.0"))),
      P("b", needs = Vector(Need.of(sink, "^1.0"))))
    assertEquals(ps, Vector(
      Problem.Missing("a", "clock", Range("^1.0")),
      Problem.Missing("b", "sink", Range("^1.0"))))
  }

  test("two plugins with one id") {
    assertEquals(problems(P("a"), P("a", version = Version("2.0"))),
      Vector(Problem.DuplicateId("a", Vector(Version("1.0"), Version("2.0")))))
  }

  test("a plugin built for another kernel API") {
    assertEquals(problems(P("old", kernel = Range("^0.9"))),
      Vector(Problem.KernelMismatch("old", Range("^0.9"), Kernel.api)))
  }

  test("an optional need nobody provides is fine, and maybe answers None") {
    val p = Kernel.plan(Seq(P("a", needs = Vector(Need.maybe(clock, "^1.0")),
      provides = Vector(Provision.value(sink, "1.0")(w => new Sink {
        val seen = w.maybe(clock).map(_.now)
        def put(s: String) = ()
        override def toString = seen.toString
      })))))
    val (r, close) = Resource.open(Kernel.start(p.toOption.get))
    assertEquals(r.one(sink).toString, "None")
    close()
  }

  test("a provision built against another major, or newer than the host: Incompatible") {
    assertEquals(problems(P("x", provides = Vector(clockAt(1, built = "2.0")))),
      Vector(Problem.Incompatible("x", "clock", Version("2.0"), Version("1.2"))))
    assertEquals(problems(P("y", provides = Vector(clockAt(1, built = "1.3")))),
      Vector(Problem.Incompatible("y", "clock", Version("1.3"), Version("1.2"))))
    assert(Kernel.plan(Seq(P("z", provides = Vector(clockAt(1, built = "1.1"))))).isRight,
      "an older minor of the same major is served")
  }

  test("providers exist and none is in the need's range: Unserved, with each candidate") {
    assertEquals(problems(
      P("old", provides = Vector(clockAt(1, built = "1.0"))),
      P("user", needs = Vector(Need.of(clock, "^1.2")))),
      Vector(Problem.Unserved("user", "clock", Range("^1.2"), Vector(("old", Version("1.0"))))))
  }

  test("a One port with two providers is Ambiguous until a choice names one") {
    val ps = Seq(P("a", provides = Vector(clockAt(1))), P("b", provides = Vector(clockAt(2))),
      P("user", needs = Vector(Need.of(clock, "^1.0"))))
    assertEquals(Kernel.plan(ps).left.toOption.get, Vector(Problem.Ambiguous("clock", Vector("a", "b"))))
    val plan = Kernel.plan(ps, choose = Map("clock" -> "b")).toOption.get
    val (r, close) = Resource.open(Kernel.start(plan))
    assertEquals(r.one(clock).now, 2L)
    close()
    assertEquals(Kernel.plan(ps, choose = Map("clock" -> "c")).left.toOption.get,
      Vector(Problem.UnknownChoice("clock", "c"), Problem.Ambiguous("clock", Vector("a", "b"))))
  }

  test("a cycle is named with its path") {
    val ps = problems(
      P("a", needs = Vector(Need.of(sink, "^1.0")), provides = Vector(clockAt(1))),
      P("b", needs = Vector(Need.of(clock, "^1.0")),
        provides = Vector(Provision.value(sink, "1.0")(_ => new Sink { def put(s: String) = () }))))
    assertEquals(ps, Vector(Problem.Cycle(Vector("a", "b", "a"))))
  }

  test("the order: providers before users, by id where free, the same every run") {
    val ps = Seq(
      P("z-user", needs = Vector(Need.of(clock, "^1.0"))),
      P("m-clock", provides = Vector(clockAt(1))),
      P("a-free"))
    val once = Kernel.plan(ps).toOption.get.order.map(_.id)
    assertEquals(once, Vector("a-free", "m-clock", "z-user"))
    assertEquals(Kernel.plan(ps.reverse).toOption.get.order.map(_.id), once)
  }

  test("disabled removes a plugin first; what then goes missing is reported") {
    val ps = Seq(P("c", provides = Vector(clockAt(1))), P("u", needs = Vector(Need.of(clock, "^1.0"))))
    assertEquals(Kernel.plan(ps, disabled = Set("c")).left.toOption.get,
      Vector(Problem.Missing("u", "clock", Range("^1.0"))))
  }

  test("two Port values with one name and different versions conflict") {
    val other: Port[Clock] = Port.one("clock", Version("2.0"))
    val ps = problems(P("a", provides = Vector(clockAt(1))), P("b", needs = Vector(Need.of(other, "^2.0"))))
    assert(ps.contains(Problem.PortConflict("clock", Vector(Version("1.2"), Version("2.0")))), ps)
  }

  test("start makes in plan order and releases in reverse, once") {
    val log = mutable.ArrayBuffer.empty[String]
    def src(n: String): Provision[Source] = Provision(sources, Version("2.0"), _ =>
      Resource.acquire { log += s"open $n"; new Source { def name = n } }(_ => log += s"close $n"))
    val sinkP = Provision(sink, Version("1.0"), w =>
      Resource.acquire { log += s"open sink over ${w.all(sources).map(_.name).mkString("+")}"
        new Sink { def put(s: String) = () } }(_ => log += "close sink"))
    val plan = Kernel.plan(Seq(
      P("out", needs = Vector(Need.of(sources, "^2.0")), provides = Vector(sinkP)),
      P("s1", provides = Vector(src("s1"))), P("s2", provides = Vector(src("s2"))))).toOption.get
    val (r, close) = Resource.open(Kernel.start(plan))
    assertEquals(r.all(sources).map(_.name), Vector("s1", "s2"))
    assertEquals(r.installed.map(i => s"${i.plugin}:${i.port}"), Vector("s1:source", "s2:source", "out:sink"))
    close(); close()
    assertEquals(log.toVector, Vector("open s1", "open s2", "open sink over s1+s2",
      "close sink", "close s2", "close s1"))
  }

  test("a broken law stops the start, names the law, and releases what was made") {
    val log = mutable.ArrayBuffer.empty[String]
    val good = Provision(clock, Version("1.2"), _ =>
      Resource.acquire { log += "open clock"; new Clock { def now = 1L } }(_ => log += "close clock"))
    val plan = Kernel.plan(Seq(P("a", provides = Vector(good)),
      P("b", provides = Vector(source(""))))).toOption.get
    val e = intercept[Refused](Resource.open(Kernel.start(plan)))
    assertEquals(e.problems, Vector(Problem.LawBroken("b", "source", "a source has a name",
      "a source has a name does not hold")))
    assertEquals(log.toVector, Vector("open clock", "close clock"))
    // verify = false starts it anyway
    val (_, close) = Resource.open(Kernel.start(plan, verify = false))
    close()
  }

  test("a plugin reads only what it declared") {
    val sneaky = Provision.value(sink, "1.0")(w => { val _ = w.one(clock); new Sink { def put(s: String) = () } })
    val plan = Kernel.plan(Seq(P("c", provides = Vector(clockAt(1))),
      P("s", provides = Vector(sneaky)))).toOption.get
    val e = intercept[IllegalStateException](Resource.open(Kernel.start(plan)))
    assert(e.getMessage.contains("s reads clock"), e.getMessage)
  }

  test("assemble refuses a plan with problems, with every sentence") {
    val e = intercept[Refused](Kernel.assemble(Seq(P("a", needs = Vector(Need.of(clock, "^1.0"))))))
    assertEquals(e.getMessage, "a needs clock ^1.0.0 and nothing provides it")
  }
