package okay.audit

import java.nio.file.{Files, Path, Paths}

class TestAudit extends munit.FunSuite with okay.testkit.Munit.Diagnosed:
  test("JPMS descriptors distinguish SQL from java.base and unresolved transitive readability"):
    def descriptor(source: String): Path =
      val dir = Files.createTempDirectory("audit-module")
      val file = dir.resolve("module-info.java")
      Files.writeString(file, source): Unit
      val compiler = javax.tools.ToolProvider.getSystemJavaCompiler
      assert(compiler != null, "descriptor fixture requires a JDK")
      assertEquals(compiler.run(null, null, null, "-d", dir.toString, file.toString), 0)
      dir
    val isolated = descriptor("module fixture.isolated {}")
    val sql = descriptor("module fixture.sql { requires java.sql; }")
    val evidence = Jpms.evidence(Vector("isolated" -> isolated, "sql" -> sql))
    note(evidence.toString)
    val states = evidence.modules.map(m => m.module -> m.enforcement.toMap).toMap
    assertEquals(states("isolated")("java.sql."), Enforcement.JvmEnforced)
    assertEquals(states("sql")("java.sql."), Enforcement.ScanOnly)
    assertEquals(states("isolated")("java.net."), Enforcement.ScanOnly)
    assertEquals(states("isolated")("sun."), Enforcement.ScanOnly)
    val overridden = Jpms.evidence(Vector("isolated" -> isolated), Vector("--add-reads=fixture.isolated=java.sql"))
    assertEquals(overridden.modules.head.enforcement.toMap.apply("java.sql."), Enforcement.ScanOnly)
    val jarPath = Files.createTempFile("audit-module", ".jar")
    val jar = new java.util.jar.JarOutputStream(Files.newOutputStream(jarPath))
    try
      jar.putNextEntry(new java.util.jar.JarEntry("module-info.class"))
      jar.write(Files.readAllBytes(isolated.resolve("module-info.class")))
      jar.closeEntry()
    finally jar.close()
    assertEquals(Jpms.evidence(Vector("jar" -> jarPath)).modules.head.name, Some("fixture.isolated"))

  test("JPMS split packages identify distinct class inputs, not repeated classpaths"):
    val first = only("Pure")
    val second = only("Sockets")
    val report = Audit.run(Boundary(Map("a" -> Layer.Handlers, "b" -> Layer.Handlers)), Map("a" -> Seq(first), "b" -> Seq(second)))
    assertEquals(report.jpms.splits.map(_.name), Vector("fixture"))
    assert(report.text.contains("split package fixture"))
    assert(report.json.contains("\"package\":\"fixture\""))
    assertEquals(Jpms.evidence(Vector("a" -> first, "b" -> first)).splits, Vector.empty)

  test("JPMS launcher options resolve beside the manifest and runtime evidence includes java.base"):
    val dir = Files.createTempDirectory("audit-options")
    Files.writeString(dir.resolve("jvm.options"), "--add-opens=java.base/java.lang=ALL-UNNAMED\n-Xmx1g\n--illegal-native-access=deny\n"): Unit
    val manifest = dir.resolve("audit.json")
    Files.writeString(manifest, """{"modules":[],"jvmOptions":"jvm.options"}"""): Unit
    val input = Manifest.read(manifest)
    assertEquals(input.jvmOptions.size, 2)
    val report = Audit.run(Boundary(Map.empty), Map.empty, input.jvmOptions)
    assert(report.json.contains("--illegal-native-access=deny"))
    assert(Audit.runtime().modules.exists(_.name == "java.base"))
    assertEquals(Audit.runtime().inputArguments, java.lang.management.ManagementFactory.getRuntimeMXBean.getInputArguments.toArray.toVector.map(_.toString).sorted)
    val missingOptions = intercept[Audit.Refused](Jpms.launcherOptions(dir.resolve("missing.options")))
    assert(missingOptions.getMessage.contains("missing.options"))
    Files.writeString(dir.resolve("jvm.options"), "--add-opens\n"): Unit
    val invalidOptions = intercept[Audit.Refused](Manifest.read(manifest))
    assert(invalidOptions.getMessage.contains("needs a value"))
    val broken = dir.resolve("module-info.class")
    Files.write(broken, Array[Byte](0, 1)): Unit
    val invalidDescriptor = intercept[Audit.Refused](Audit.run(Boundary(Map("broken" -> Layer.Handlers)), Map("broken" -> Seq(dir))))
    assert(invalidDescriptor.getMessage.contains("JPMS descriptor"))
  /** the test classes directory holding the fixture classes; `only` copies the named ones into a temp dir */
  val classesDir: Path = Paths.get(getClass.getResource("/fixture/Pure.class").toURI).getParent.getParent

  def only(names: String*): Path =
    val dir = Files.createTempDirectory("audit-fixture")
    Files.createDirectories(dir.resolve("fixture"))
    names.foreach { n =>
      Files.list(classesDir.resolve("fixture")).filter(p => p.getFileName.toString.matches(s"$n(\\$$.*)?\\.class"))
        .forEach(p => Files.copy(p, dir.resolve("fixture").resolve(p.getFileName.toString)): Unit)
    }
    if names.contains("io/Door") then
      Files.createDirectories(dir.resolve("fixture/io"))
      Files.copy(classesDir.resolve("fixture/io/Door.class"), dir.resolve("fixture/io/Door.class")): Unit
    dir

  def business(paths: Path*): Report =
    Audit.run(Boundary(Map("biz" -> Layer.Business)), Map("biz" -> paths))

  test("a business class that constructs java.net.Socket is a finding naming the class, the member, the API and the why; the build fails"):
    val r = business(only("Sockets"))
    assert(!r.passed)
    val f = r.findings.find(_.ref.member == "java.net.Socket#<init>").getOrElse(fail(r.text))
    assertEquals(f.ref.from, "fixture.Sockets")
    assertEquals(f.rule.api, "java.net.")
    assert(f.rule.why.startsWith("network"))
    assert(r.text.contains("audit: FAIL"))
    assert(r.text.contains("fixture.Sockets -> java.net.Socket#<init>  [java.net.: network"))

  test("a lambda, a string concatenation and a record in a business class are NOT findings — the bootstraps are the carve-out"):
    val r = business(only("Lambdas"))
    assert(r.passed, r.text)
    // and they were really there: the scan saw the bootstrap owners before the carve-out
    val raw = Scan.directory(only("Lambdas")).map(_.owner).toSet
    assert(raw.contains("java.lang.invoke.LambdaMetafactory"), raw.toString)
    assert(raw.contains("java.lang.invoke.StringConcatFactory") || raw.contains("java.lang.StringBuilder"), raw.toString)
    assert(raw.contains("java.lang.runtime.ObjectMethods"), raw.toString)
    assert(r.text.contains("carve-out (compiler bootstraps, never a finding): java.lang.invoke.LambdaMetafactory"))

  test("Class.forName, Lookup.findStatic, ObjectInputStream and a native method are findings — the escape hatches are closed"):
    val r = business(only("Reflect", "Natives"))
    val members = r.findings.map(_.ref.member).toSet
    assert(members.contains("java.lang.Class#forName"), members.toString)
    assert(members.contains("java.lang.invoke.MethodHandles$Lookup#findStatic"), members.toString)
    assert(!members.contains("java.lang.invoke.MethodHandles#lookup"), "lookup() alone reaches nothing: " + members)
    assert(members.contains("java.io.ObjectInputStream#<init>"), members.toString)
    val native = r.findings.find(_.ref.kind == Ref.Kind.Native).getOrElse(fail(r.text))
    assertEquals(native.ref.member, "fixture.Natives#tick")
    assert(r.text.contains("fixture.Natives -> fixture.Natives#tick (native method)"))

  test("time and randomness: currentTimeMillis, new Date() and Random are findings with the replay reason"):
    val r = business(only("Clocky"))
    val members = r.findings.map(_.ref.member).toSet
    assertEquals(members, Set("java.lang.System#currentTimeMillis", "java.util.Date#<init>", "java.util.Random#<init>", "java.util.Random#nextInt", "java.util.Random"))
    assert(r.findings.forall(_.rule.why.startsWith("time and randomness")))

  test("a Scala 3 lazy val — lookup, findVarHandle, VarHandle.compareAndSet — is the compiler's idiom, not a reach"):
    val raw = Scan.directory(only("Lazy")).map(_.member).toSet
    assert(raw.contains("java.lang.invoke.MethodHandles$Lookup#findVarHandle"), raw.toString)
    val r = business(only("Lazy"))
    assert(r.passed, r.text)
    assert(r.text.contains("the lazy-val idiom java.lang.invoke.MethodHandles#lookup, java.lang.invoke.MethodHandles$Lookup#findVarHandle, java.lang.invoke.VarHandle"))

  test("a pure class passes, and Pure is listed in no inventory because it is business"):
    val r = business(only("Pure"))
    assert(r.passed, r.text)
    assertEquals(r.inventory, Vector.empty)

  test("a jar on a business classpath is scanned under the business rules; the finding names the jar in the inputs with its sha-256"):
    val dir = only("Sockets", "Pure")
    val jar = Files.createTempFile("audit", ".jar")
    val out = java.util.jar.JarOutputStream(Files.newOutputStream(jar))
    Files.list(dir.resolve("fixture")).forEach { p =>
      out.putNextEntry(java.util.jar.JarEntry("fixture/" + p.getFileName)); out.write(Files.readAllBytes(p)); out.closeEntry()
    }
    out.close()
    val r = business(jar)
    assert(!r.passed)
    assertEquals(r.findings.map(_.ref.from).distinct, Vector("fixture.Sockets"))
    val in = r.inputs.find(_._2 == jar).getOrElse(fail("jar not in inputs"))
    assertEquals(in._3, Scan.digest(jar))
    assertEquals(in._3.length, 64)

  test("a handler module's references are LISTED by provider, never failed; the runtime likewise; a module without a layer is untracked"):
    val r = Audit.run(
      Boundary(Map("io" -> Layer.Handlers, "core" -> Layer.Runtime)),
      Map("io" -> Seq(only("Sockets", "Reflect")), "core" -> Seq(only("Clocky")), "nobody" -> Seq(only("Sockets"))))
    assert(r.passed)
    val io = r.inventory.find(_.module == "io").getOrElse(fail("io missing"))
    assertEquals(io.layer, Layer.Handlers)
    assertEquals(io.byProvider.keySet, Set("java.net", "java.lang", "java.io"))
    assert(io.byProvider("java.net").map(_.member).contains("java.net.Socket#<init>"))
    val core = r.inventory.find(_.module == "core").getOrElse(fail("core missing"))
    assertEquals(core.layer, Layer.Runtime)
    assertEquals(core.byProvider.keySet, Set("java.lang", "java.util"))
    assertEquals(r.untracked, Vector("nobody"))
    assert(r.text.contains("io [handlers]"))
    assert(r.text.contains("UNTRACKED (no layer declared): nobody"))

  test("an allow needs a reason and an owner — one without is refused by name before any scan; a matched allow is listed, an unmatched one is unused"):
    val bad = Boundary(Map("biz" -> Layer.Business), allows = Vector(Allow("biz", "java.net.", "", "ops")))
    val e = intercept[Audit.Refused](Audit.run(bad, Map("biz" -> Seq(only("Sockets")))))
    assert(e.getMessage.contains("allow biz: java.net. has no reason"))
    val ok = Boundary(Map("biz" -> Layer.Business), allows = Vector(
      Allow("biz", "java.net.Socket", "legacy health probe, removed with ticket X", "ops"),
      Allow("biz", "java.sql.", "never used", "ops")))
    val r = Audit.run(ok, Map("biz" -> Seq(only("Sockets"))))
    assert(r.passed, r.text)
    assertEquals(r.allowed.map(_._2.ref.member).toSet, Set("java.net.Socket#<init>", "java.net.Socket", "java.net.Socket#getPort", "java.net.Socket#close"))
    assertEquals(r.unusedAllows.map(_.api), Vector("java.sql."))
    assert(r.text.contains("ALLOWED (named exceptions)"))
    assert(r.text.contains("UNUSED ALLOWS"))

  test("text and json are deterministic for the same inputs; json is well-formed enough to round-trip its keys"):
    val a = only("Sockets", "Clocky", "Lambdas")
    val r1 = business(a); val r2 = business(a)
    assertEquals(r1.text, r2.text)
    assertEquals(r1.json, r2.json)
    assert(r1.json.startsWith("""{"passed":false,"bootstraps":["""))
    for key <- Seq("findings", "allowed", "unusedAllows", "inventory", "untracked", "inputs") do
      assert(r1.json.contains("\"" + key + "\":"), key)

  test("rules: a package prefix, a class with its nested classes, a member, a name prefix, a pinned descriptor"):
    def ref(owner: String, name: String = "", desc: String = "") = Ref("x.Y", if name.isEmpty then Ref.Kind.Class else Ref.Kind.Method, owner, name, desc)
    assert(Rule("java.net.", "").matches(ref("java.net.Socket")))
    assert(!Rule("java.net.", "").matches(ref("java.network.Thing")))
    assert(Rule("java.lang.Thread", "").matches(ref("java.lang.Thread$Builder", "start", "()V")))
    assert(!Rule("java.lang.Thread", "").matches(ref("java.lang.ThreadLocal", "get", "()Ljava/lang/Object;")))
    assert(Rule("java.lang.System#exit", "").matches(ref("java.lang.System", "exit", "(I)V")))
    assert(!Rule("java.lang.System#exit", "").matches(ref("java.lang.System", "exit0", "(I)V")))
    assert(Rule("java.time.Clock#system*", "").matches(ref("java.time.Clock", "systemUTC", "()Ljava/time/Clock;")))
    assert(Rule("java.util.Date#<init>()", "").matches(ref("java.util.Date", "<init>", "()V")))
    assert(!Rule("java.util.Date#<init>()", "").matches(ref("java.util.Date", "<init>", "(J)V")))
    assertEquals(Audit.provider("java.sql.Connection"), "java.sql")
    assertEquals(Audit.provider("com.zaxxer.hikari.HikariDataSource"), "com.zaxxer.hikari")
    assertEquals(Audit.provider("okay.Async"), "okay")

  test("PACKAGES: a business module whose `fixture.io` package is its handler layer passes, and the package is listed as `biz:fixture.io`; without the prefix it fails"):
    val dir = only("Pure", "io/Door")
    val plain = business(dir)
    assert(!plain.passed, plain.text)
    val r = Audit.run(Boundary(Map("biz" -> Layer.Business), packages = Map("biz" -> Map("fixture.io" -> Layer.Handlers))), Map("biz" -> Seq(dir)))
    assert(r.passed, r.text)
    val inv = r.inventory.find(_.module == "biz:fixture.io").getOrElse(fail(r.text))
    assertEquals(inv.layer, Layer.Handlers)
    assert(inv.byProvider("java.net").exists(_.member == "java.net.Socket#<init>"))
    assert(r.text.contains("biz:fixture.io [handlers]"))
    // the longest prefix wins: `fixture` as business, `fixture.io` as handlers
    val r2 = Audit.run(Boundary(Map.empty, packages = Map("biz" -> Map("fixture" -> Layer.Business, "fixture.io" -> Layer.Handlers))), Map("biz" -> Seq(dir)))
    assert(r2.passed, r2.text)
    assertEquals(r2.untracked, Vector.empty)
    assertEquals(r2.inventory.map(_.module), Vector("biz:fixture.io"))

    val other = Audit.run(Boundary(Map("biz" -> Layer.Business, "other" -> Layer.Business), packages = Map("biz" -> Map("fixture.io" -> Layer.Handlers))), Map("biz" -> Seq(dir), "other" -> Seq(dir)))
    assert(!other.passed, other.text)
    assert(other.findings.exists(_.module == "other"), other.text)

  test("Main: the TSV manifest in, report.txt and report.json out"):
    val dir = Files.createTempDirectory("audit-main")
    val manifest = dir.resolve("modules.tsv")
    Files.writeString(manifest, s"biz\tbusiness\t${only("Pure", "io/Door")}\tfixture.io=handlers\nio\thandlers\t${only("Sockets")}\nnobody\tuntracked\t\n")
    val allows = dir.resolve("allows.tsv")
    Files.writeString(allows, "biz\tjava.sql.\tops\tnever\n")
    val (layers, modules, packages) = Main.readManifest(manifest)
    assertEquals(layers, Map("biz" -> Layer.Business, "io" -> Layer.Handlers, "nobody" -> Layer.Untracked))
    assertEquals(modules("nobody"), Seq.empty)
    assertEquals(packages, Map("biz" -> Map("fixture.io" -> Layer.Handlers), "io" -> Map.empty, "nobody" -> Map.empty))
    val r = Audit.run(Boundary(layers, allows = Main.readAllows(allows), packages = packages), modules)
    assert(r.passed, r.text)
    assertEquals(r.unusedAllows.size, 1)
    assertEquals(r.untracked, Vector("nobody"))

  test("Main: the standalone JSON manifest resolves paths beside itself, writes both reports, and returns one for a finding"):
    val classes = only("Sockets")
    val root = classes.getParent
    val manifest = root.resolve("audit.json")
    Files.writeString(manifest,
      s"""{"modules":[{"name":"biz","layer":"business","paths":["${classes.getFileName}"]}]}""")
    val out = root.resolve("report")
    assertEquals(Main.run(Array("--manifest", manifest.toString, "--report", out.toString)), 1)
    assert(Files.readString(out.resolve("report.txt")).contains("fixture.Sockets"))
    assert(Files.readString(out.resolve("report.json")).contains("\"passed\":false"))

  test("Main: a JSON package handler is inventoried while its business sibling stays checked"):
    val classes = only("Pure", "io/Door")
    val root = classes.getParent
    val manifest = root.resolve("audit-packages.json")
    Files.writeString(manifest,
      s"""{"modules":[{"name":"biz","layer":"business","paths":["${classes.getFileName}"],"packages":{"fixture.io":"handlers"}}]}""")
    val out = root.resolve("report-packages")
    assertEquals(Main.run(Array("--manifest", manifest.toString, "--report", out.toString)), 0)
    assert(Files.readString(out.resolve("report.txt")).contains("biz:fixture.io [handlers]"))

  test("Main: malformed standalone JSON is refused by name before scanning"):
    val dir = Files.createTempDirectory("audit-json-refusal")
    val unknown = dir.resolve("unknown.json")
    Files.writeString(unknown, """{"modules":[{"name":"biz","layer":"pure","paths":["missing"]}]}""")
    assertEquals(Main.run(Array("--manifest", unknown.toString, "--report", dir.resolve("out").toString)), 2)
    val malformed = dir.resolve("malformed.json")
    Files.writeString(malformed, "{" )
    assertEquals(Main.run(Array("--manifest", malformed.toString, "--report", dir.resolve("out2").toString)), 2)
    val duplicate = dir.resolve("duplicate.json")
    Files.writeString(duplicate,
      """{"modules":[{"name":"biz","layer":"business","paths":["."]},{"name":"biz","layer":"business","paths":["."]}]}""")
    assertEquals(Main.run(Array("--manifest", duplicate.toString, "--report", dir.resolve("out3").toString)), 2)
    val missingPaths = dir.resolve("missing-paths.json")
    Files.writeString(missingPaths, """{"modules":[{"name":"biz","layer":"business"}]}""")
    assertEquals(Main.run(Array("--manifest", missingPaths.toString, "--report", dir.resolve("out4").toString)), 2)
    val nonStringPath = dir.resolve("non-string-path.json")
    Files.writeString(nonStringPath, """{"modules":[{"name":"biz","layer":"business","paths":[true]}]}""")
    assertEquals(Main.run(Array("--manifest", nonStringPath.toString, "--report", dir.resolve("out5").toString)), 2)
