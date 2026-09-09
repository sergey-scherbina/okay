package okay.r

import RValue.*

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}

/**
 * Stage 0 against a LIVE R (specs/r.md): addressing, R's TWO
 * absences, conditions that leave the process alive, the clean
 * environment, frames, the handshake refusals — the shim version and
 * the named jsonlite prerequisite — and the dead-process throw.
 *
 * R is reached through a container when it is not installed on the
 * machine, by a shim at the SAME absolute path inside and out, so
 * `Rscript` means the same thing on both sides. That is the only
 * difference from okay-py's suite, and it is not a difference in what
 * is proven.
 */
object TestR:

  private val image = "okay-r-test"

  /** R on the PATH if there is one; otherwise a shim that runs the
   * container's, built once and cached */
  lazy val rscript: Option[String] =
    onPath("Rscript").orElse(containerShim())

  private def onPath(bin: String): Option[String] =
    sys.env.getOrElse("PATH", "").split(":").iterator
      .map(d => Path.of(d, bin))
      .find(Files.isExecutable(_)).map(_.toString)

  private def sh(cmd: Vector[String]): Int =
    try
      val p = ProcessBuilder(cmd*).redirectErrorStream(true).start()
      p.getInputStream.readAllBytes(): Unit
      p.waitFor()
    catch case _: Exception => 127

  private def containerShim(): Option[String] =
    if onPath("docker").isEmpty || sh(Vector("docker", "version")) != 0 then None
    else
      val built =
        sh(Vector("docker", "image", "inspect", image)) == 0 || {
          val dir = Files.createTempDirectory("okay-r-image")
          Files.writeString(dir.resolve("Dockerfile"),
            "FROM r-base:4.4.1\n" +
              "RUN apt-get update -qq && apt-get install -y --no-install-recommends " +
              "r-cran-jsonlite && rm -rf /var/lib/apt/lists/*\n", UTF_8): Unit
          sh(Vector("docker", "build", "-t", image, dir.toString)) == 0
        }
      Option.when(built) {
        // the shim lives where the temp files do, and mounts that
        // whole directory: a path the host passes means the same
        // thing to the R inside
        val tmp = System.getProperty("java.io.tmpdir")
        val shim = Files.createTempFile("okay-r-rscript", ".sh")
        Files.writeString(shim,
          s"""#!/bin/sh
             |# A shim standing in for Rscript must behave like it, and
             |# Rscript INHERITS its parent's environment while `docker
             |# run` builds a fresh one. Without forwarding, the
             |# clean-env tests would measure docker rather than us.
             |envfile=$$(mktemp)
             |env > "$$envfile"
             |docker run --rm -i --env-file "$$envfile" -v $tmp:$tmp -w $tmp $image Rscript "$$@"
             |status=$$?
             |rm -f "$$envfile"
             |exit $$status
             |""".stripMargin, UTF_8): Unit
        shim.toFile.setExecutable(true): Unit
        shim.toFile.deleteOnExit()
        shim.toString
      }

class TestR extends munit.FunSuite {

  // integration-test-gate: out of the default gate, into `sbt integrationTest`
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))

  override def munitIgnore: Boolean = TestR.rscript.isEmpty

  override val munitTimeout = scala.concurrent.duration.Duration(10, "min")

  private var sessions = List.empty[RSubprocess]
  private def session(env: Map[String, String] = Map.empty,
                      timeoutMillis: Option[Long] = None): RSubprocess =
    val s = RSubprocess.start(TestR.rscript.get, env, timeoutMillis)
    sessions = s :: sessions
    s
  override def afterAll(): Unit = sessions.foreach(_.close())

  private def call(s: RSubprocess, fn: String, args: RValue*) =
    s.handler.handle(REval.Call(fn, args.toVector))

  // ---- addressing -----------------------------------------------------

  test("pkg::name addressing, a base name, and the program is data rather than code") {
    val r = session()
    assertEquals(call(r, "sqrt", Vec(Vector(F64(9)))), Right(Vec(Vector(F64(3)))))
    assertEquals(call(r, "stats::median", Vec(Vector(F64(3), F64(1), F64(2)))),
      Right(Vec(Vector(F64(2)))))
    // there is no operation that evals a string: the enum has two
    // cases and neither takes source, which is structural rather than
    // a check anyone has to remember to run
    assertEquals(REval.Call("f", Vector.empty).ordinal, 0)
    assertEquals(REval.Frame("f", RFrame(Vector.empty), Vector.empty).ordinal, 1)
  }

  test("a missing function and a missing package are CONDITIONS, and the process survives") {
    val r = session()
    val gone = call(r, "nosuchfunction", Vec(Vector.empty))
    assert(gone.left.exists(_.message.contains("nosuchfunction")), gone.toString)
    val pkg = call(r, "nosuchpkg::f", Vec(Vector.empty))
    assert(pkg.left.exists(_.message.contains("nosuchpkg")), pkg.toString)
    // still alive, which is the whole point of a condition being data
    assertEquals(call(r, "sqrt", Vec(Vector(F64(16)))), Right(Vec(Vector(F64(4)))))
  }

  test("stop() surfaces as a condition with its message, and the next call still works") {
    val r = session()
    val boom = call(r, "stop", Str("deliberate"))
    assert(boom.left.exists(_.message.contains("deliberate")), boom.toString)
    assert(boom.left.exists(_.kind.nonEmpty), boom.toString)
    assertEquals(call(r, "sqrt", Vec(Vector(F64(25)))), Right(Vec(Vector(F64(5)))))
  }

  // ---- R's two absences, which is what makes this not a copy ----------

  test("NULL and NA are DIFFERENT, and R's own arithmetic proves why it matters") {
    val r = session()
    // mean(c(1, 2, NA)) is NA — the missing value poisons the mean
    assertEquals(call(r, "mean", Vec(Vector(F64(1), F64(2), NA(RType.Double)))),
      Right(Vec(Vector(NA(RType.Double)))))
    // mean(c(1, 2)) is 1.5 — and a NULL would have VANISHED from the
    // vector rather than poisoning it
    assertEquals(call(r, "mean", Vec(Vector(F64(1), F64(2)))), Right(Vec(Vector(F64(1.5)))))
    // NULL is the absence of an object: length(NULL) is 0
    assertEquals(call(r, "length", RNull), Right(Vec(Vector(I32(0)))))
    // is.null and is.na are different questions
    assertEquals(call(r, "is.null", RNull), Right(Vec(Vector(Bool(true)))))
    assertEquals(call(r, "is.na", Vec(Vector(NA(RType.Integer)))), Right(Vec(Vector(Bool(true)))))
  }

  test("an NA keeps its TYPE across the wire, because R's four NAs are four values") {
    val r = session()
    for t <- RType.values do
      val back = call(r, "identity", Vec(Vector(NA(t))))
      assertEquals(back, Right(Vec(Vector(NA(t)))), s"NA of $t")
    // and the type is R's own: an integer NA is in an integer vector
    assertEquals(call(r, "class", Vec(Vector(I32(1), NA(RType.Integer)))),
      Right(Vec(Vector(Str("integer")))))
    assertEquals(call(r, "class", Vec(Vector(F64(1), NA(RType.Double)))),
      Right(Vec(Vector(Str("numeric")))))
  }

  test("NA and NaN are not the same absence either — R says so and so does the wire") {
    val r = session()
    // NaN != NaN by Java's rules, so the VALUE is what to look at
    call(r, "identity", Vec(Vector(F64(Double.NaN)))) match
      case Right(Vec(Vector(F64(d)))) => assert(d.isNaN, s"came back $d")
      case other => fail(s"not a NaN: $other")
    // is.na(NaN) is TRUE in R and that is R's rule, not ours; what
    // matters is that the VALUE came back as NaN and not as NA
    assertEquals(call(r, "is.nan", Vec(Vector(F64(Double.NaN)))), Right(Vec(Vector(Bool(true)))))
    assertEquals(call(r, "is.nan", Vec(Vector(NA(RType.Double)))), Right(Vec(Vector(Bool(false)))))
  }

  test("an integer stays an integer and a double stays a double — JSON would have merged them") {
    val r = session()
    assertEquals(call(r, "class", Vec(Vector(I32(3)))), Right(Vec(Vector(Str("integer")))))
    assertEquals(call(r, "class", Vec(Vector(F64(3)))), Right(Vec(Vector(Str("numeric")))))
    assertEquals(call(r, "identity", Vec(Vector(I32(3)))), Right(Vec(Vector(I32(3)))))
    assertEquals(call(r, "identity", Vec(Vector(F64(3)))), Right(Vec(Vector(F64(3)))))
  }

  test("raw bytes and strings survive") {
    val r = session()
    val bs = Array[Byte](1, 2, 3)
    call(r, "identity", Bytes(bs)) match
      case Right(Bytes(back)) => assertEquals(back.toVector, bs.toVector)
      case other => fail(s"not raw: $other")
    assertEquals(call(r, "toupper", Vec(Vector(Str("ok")))), Right(Vec(Vector(Str("OK")))))
  }

  // ---- frames ---------------------------------------------------------

  test("a frame goes out as columns and comes back as columns, order and count intact") {
    val r = session()
    val in = RFrame(Vector(
      "x" -> Vector(F64(1), F64(2), F64(3)),
      "s" -> Vector(Str("a"), Str("b"), Str("c"))))
    assertEquals(r.handler.handle(REval.Frame("identity", in, Vector.empty)), Right(in))
  }

  test("a frame column carries NA in place, which is the whole reason an analyst has a frame") {
    val r = session()
    val in = RFrame(Vector("x" -> Vector(F64(1), NA(RType.Double), F64(3))))
    assertEquals(r.handler.handle(REval.Frame("identity", in, Vector.empty)), Right(in))
    // and R agrees it is missing
    assertEquals(call(r, "sum", Vec(Vector(F64(1), NA(RType.Double), F64(3)))),
      Right(Vec(Vector(NA(RType.Double)))))
  }

  test("a frame function that answers something else is a condition naming what it answered") {
    val r = session()
    val in = RFrame(Vector("x" -> Vector(F64(1))))
    val bad = r.handler.handle(REval.Frame("nrow", in, Vector.empty))
    assert(bad.left.exists(_.message.contains("must answer a data.frame")), bad.toString)
  }

  // ---- the environment ------------------------------------------------

  test("the R process sees EXACTLY what the config names, and nothing else we passed") {
    val named = session(Map("OKAY_R_NAMED" -> "yes"))
    assertEquals(call(named, "Sys.getenv", Vec(Vector(Str("OKAY_R_NAMED")))),
      Right(Vec(Vector(Str("yes")))))
    // a second session that does NOT name it does not have it: the
    // child's environment is the map and only the map
    val bare = session()
    assertEquals(call(bare, "Sys.getenv", Vec(Vector(Str("OKAY_R_NAMED")))),
      Right(Vec(Vector(Str("")))))
  }

  test("a real parent variable is invisible in R — checked only against R on the PATH") {
    // `docker run` builds its OWN environment (HOME=/root and so on)
    // regardless of what we cleared, so under the container shim this
    // assertion would be testing docker rather than the clean-env
    // rule. The rule itself is `pb.environment().clear()` and the
    // test above exercises it either way.
    assume(TestR.rscript.exists(!_.endsWith(".sh")), "R is reached through a container here")
    val r = session()
    assertEquals(call(r, "Sys.getenv", Vec(Vector(Str("HOME")))), Right(Vec(Vector(Str("")))))
  }

  // ---- verify ---------------------------------------------------------

  test("verify names a missing package and a version mismatch; a passing one says nothing") {
    val r = session()
    assertEquals(r.verify(Map.empty), Vector.empty)
    val missing = r.verify(Map("nosuchpackage" -> "1"))
    assertEquals(missing.length, 1)
    assert(missing.head.contains("nosuchpackage") && missing.head.contains("MISSING"), missing.head)
    // jsonlite is there by construction — the shim would not have
    // started otherwise
    assertEquals(r.verify(Map("jsonlite" -> "")), Vector.empty)
    val wrong = r.verify(Map("jsonlite" -> "99."))
    assertEquals(wrong.length, 1)
    assert(wrong.head.contains("jsonlite") && wrong.head.contains("wanted 99."), wrong.head)
  }

  // ---- the refusals ---------------------------------------------------

  test("a shim from another version is refused loudly rather than guessed at") {
    val other = java.nio.file.Files.createTempFile("okay-r-shim-v99", ".R")
    java.nio.file.Files.writeString(other,
      """cat('{"shim":99,"r":"4.0.0"}', "\n", sep = "")
        |flush(stdout())
        |while (length(readLines(file("stdin"), n = 1L, warn = FALSE)) > 0) {}
        |""".stripMargin): Unit
    val thrown = intercept[IllegalStateException](
      RSubprocess.startWith(TestR.rscript.get, other, Map.empty))
    assert(thrown.getMessage.contains("v99"), thrown.getMessage)
    assert(thrown.getMessage.contains("refuse rather than guess"), thrown.getMessage)
  }

  test("a shim without jsonlite refuses BY NAME, with the commands that fix it") {
    val faking = java.nio.file.Files.createTempFile("okay-r-shim-nojson", ".R")
    // exactly what the real shim emits when requireNamespace fails
    java.nio.file.Files.writeString(faking,
      """cat('{"shim":1,"fatal":"okay-r needs the jsonlite package and it is not installed - ',
        |    'install.packages(\\"jsonlite\\"), or your distribution\\u0027s r-cran-jsonlite"}', "\n", sep = "")
        |flush(stdout())
        |""".stripMargin): Unit
    val thrown = intercept[IllegalStateException](
      RSubprocess.startWith(TestR.rscript.get, faking, Map.empty))
    assert(thrown.getMessage.contains("jsonlite"), thrown.getMessage)
    assert(thrown.getMessage.contains("install.packages"), thrown.getMessage)
    assert(thrown.getMessage.contains("r-cran-jsonlite"), thrown.getMessage)
  }

  test("an interpreter that is not there refuses at start — the wrong-environment story") {
    val thrown = intercept[IllegalStateException](RSubprocess.start("/no/such/Rscript"))
    assert(thrown.getMessage.contains("/no/such/Rscript"), thrown.getMessage)
    assert(thrown.getMessage.contains("wrong-environment refusal"), thrown.getMessage)
  }

  test("a DEAD process makes the next call THROW — the supervisor decides, not us") {
    val r = RSubprocess.start(TestR.rscript.get)
    assertEquals(call(r, "sqrt", Vec(Vector(F64(4)))), Right(Vec(Vector(F64(2)))))
    r.close()
    val thrown = intercept[Exception](call(r, "sqrt", Vec(Vector(F64(4)))))
    assert(thrown.getMessage != null && thrown.getMessage.nonEmpty, "a dead process threw nothing to act on")
  }

  // ---- the timeout (r-finish, specs/r.md) -----------------------------

  test("a call that never answers is killed at the deadline, reported as DATA, and the engine takes the next call") {
    val r = session(timeoutMillis = Some(2000L))
    val started = System.nanoTime()
    // Sys.sleep is the honest hang: R is busy in C, and no polite
    // protocol can interrupt it — only the process can be stopped
    val out = call(r, "Sys.sleep", F64(120))
    val waited = (System.nanoTime() - started) / 1000000L
    out match
      case Left(c) =>
        assertEquals(c.kind, "timeout")
        assert(c.message.contains("2000ms"), c.message)
      case Right(v) => fail(s"the call answered instead of timing out: $v")
    assert(waited >= 1900L && waited < 30000L, s"waited ${waited}ms for a 2000ms deadline")
    // …and the engine is USABLE: a fresh process took the dead one's place
    assertEquals(call(r, "sqrt", Vec(Vector(F64(16)))), Right(Vec(Vector(F64(4)))))
    assertEquals(call(r, "paste", Str("a"), Str("b")), Right(Vec(Vector(Str("a b")))))
  }

  test("with no deadline set, nothing changes: the engine blocks as it always did (and answers)") {
    val r = session()
    assertEquals(r.timeoutMillis, None)
    assertEquals(call(r, "sqrt", Vec(Vector(F64(25)))), Right(Vec(Vector(F64(5)))))
  }

  test("a frame goes out and comes back as a Seq of a case class, over a REAL R") {
    final case class Point(x: Double, y: Double) derives okay.codec.Schema
    val r = session()
    val sent = Vector(Point(1.0, 2.0), Point(3.0, 4.0))
    val frame = RFrame.of(sent).fold(c => fail(s"of: $c"), identity)
    // identity in R: the frame survives the wire in both directions
    r.handler.handle(REval.Frame("identity", frame, Vector.empty)) match
      case Right(back) => assertEquals(back.rows[Point], Right(sent))
      case Left(c) => fail(s"identity on a frame: $c")
  }

  test("require: a session that names a package it must have refuses at START, naming the drift") {
    // the version is deliberately impossible, so this measures the
    // refusal and not the box's R installation
    val e = intercept[IllegalStateException](
      RSubprocess.start(TestR.rscript.get, Map.empty, None, Map("stats" -> "99.9")))
    assert(e.getMessage.contains("stats"), e.getMessage)
    assert(e.getMessage.contains("99.9"), e.getMessage)
    // …and a requirement the environment DOES meet hands the engine over
    val ok = RSubprocess.start(TestR.rscript.get, Map.empty, None, Map("stats" -> ""))
    sessions = ok :: sessions
    assertEquals(call(ok, "sqrt", Vec(Vector(F64(81)))), Right(Vec(Vector(F64(9)))))
  }

  test("the columnar wire over a REAL R: four NA types, a NaN, and an all-NA column come back intact") {
    val r = session()
    val in = RFrame(Vector(
      "l" -> Vector(Bool(true), NA(RType.Logical)),
      "i" -> Vector(I32(1), NA(RType.Integer)),
      "d" -> Vector(F64(Double.NaN), NA(RType.Double)),
      "s" -> Vector(Str("a"), NA(RType.Character)),
      "allna" -> Vector(NA(RType.Double), NA(RType.Double))))
    r.handler.handle(REval.Frame("identity", in, Vector.empty)) match
      case Right(back) =>
        assertEquals(back.cols.map(_._1), in.cols.map(_._1))
        // NaN and NA are different values in the same column, and R agrees
        assertEquals(back.cols(2)._2.head match { case F64(d) => d.isNaN; case o => fail(s"$o") }, true)
        assertEquals(back.cols(2)._2(1), NA(RType.Double))
        assertEquals(back.cols(1)._2, Vector(I32(1), NA(RType.Integer)))
        assertEquals(back.cols(3)._2, Vector(Str("a"), NA(RType.Character)))
        assertEquals(back.cols(4)._2, Vector(NA(RType.Double), NA(RType.Double)))
      case Left(c) => fail(s"identity on the frame: $c")
  }
}
