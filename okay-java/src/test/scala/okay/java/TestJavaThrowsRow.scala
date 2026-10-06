package okay.java

import okay.testkit.Munit.Diagnosed
import java.nio.file.{Files, Path}
import javax.tools.{Diagnostic, DiagnosticCollector, JavaFileObject, ToolProvider}
import scala.jdk.CollectionConverters.*

/**
 * Checked exceptions as a STATIC EFFECT ROW, as javac actually types them
 * (specs/java-direct-effects.md, the probe's first half). An effect is a
 * checked exception that is never thrown — a mark in `throws` — and a
 * handler is a method whose body type declares the effect while the
 * handler itself does not. Each case below is a Java snippet compiled by the
 * JDK's own compiler, and the test pins what javac says about it, so a
 * claim in the spec is a line here rather than a reading of JLS 18.2.5.
 */
class TestJavaThrowsRow extends munit.FunSuite, Diagnosed:

  /** three effects, a one-effect handler (`Body`), and a two-slot one (`Body2`) */
  private val base = """
    |class Counter extends Exception {}
    |class Log extends Exception {}
    |class St extends Exception {}
    |interface Body<A, R extends Exception> { A run() throws Counter, R; }
    |interface Body2<A, R1 extends Exception, R2 extends Exception> { A run() throws Counter, R1, R2; }
    |final class Fx {
    |  static int next() throws Counter { return 0; }
    |  static void log(String s) throws Log {}
    |  static void put(int s) throws St {}
    |  /** takes Counter off; whatever else the body performs is R */
    |  static <A, R extends Exception> A counter(Body<A, R> b) throws R {
    |    try { return b.run(); } catch (Counter c) { throw new IllegalStateException(c); }
    |  }
    |  static <A, R1 extends Exception, R2 extends Exception> A counter2(Body2<A, R1, R2> b) throws R1, R2 {
    |    try { return b.run(); } catch (Counter c) { throw new IllegalStateException(c); }
    |  }
    |}
    |""".stripMargin

  /** javac's errors for `base` plus one class `Case` holding `body`; empty when it compiles */
  private def javac(body: String): List[String] =
    val dir = Files.createTempDirectory("throws-row")
    val src = dir.resolve("Case.java")
    Files.writeString(src, base + s"final class Case {\n$body\n}\n")
    val compiler = ToolProvider.getSystemJavaCompiler
    val diags = DiagnosticCollector[JavaFileObject]()
    val files = compiler.getStandardFileManager(diags, null, null)
    val ok = compiler.getTask(null, files, diags, java.util.List.of("-d", dir.toString), null,
      files.getJavaFileObjects(src)).call()
    val errors = diags.getDiagnostics.asScala.toList
      .filter(_.getKind == Diagnostic.Kind.ERROR).map(_.getMessage(null))
    errors.foreach(e => note(s"javac: $e"))
    assertEquals(ok.booleanValue, errors.isEmpty, "javac's verdict and its errors disagree")
    errors

  private def refused(errors: List[String], exception: String): Unit =
    assert(errors.exists(e => e.contains("unreported exception") && e.contains(exception)),
      s"expected 'unreported exception $exception', javac said: $errors")

  test("an effect performed outside any handler does not compile"):
    refused(javac("static int g() { return Fx.next(); }"), "Counter")

  test("a handler takes its effect off, and the ONE effect left is inferred exactly"):
    assertEquals(javac("static int f() throws Log { return Fx.counter(() -> { Fx.log(\"x\"); return Fx.next(); }); }"), Nil)

  test("the effect left must still be declared: the row is checked, not dropped"):
    refused(javac("static int h() { return Fx.counter(() -> { Fx.log(\"x\"); return Fx.next(); }); }"), "Log")

  test("TWO effects left: R is their lub, Exception, so the precise row does not compile"):
    refused(javac(
      "static int k() throws Log, St { return Fx.counter(() -> { Fx.log(\"x\"); Fx.put(1); return Fx.next(); }); }"),
      "java.lang.Exception")

  test("TWO effects left, declared as Exception: compiles — the row has degraded to 'anything'"):
    assertEquals(javac(
      "static int m() throws Exception { return Fx.counter(() -> { Fx.log(\"x\"); Fx.put(1); return Fx.next(); }); }"), Nil)

  test("two slots do not help: every inference variable is bounded by every thrown type"):
    refused(javac(
      "static int k2() throws Log, St { return Fx.counter2(() -> { Fx.log(\"x\"); Fx.put(1); return Fx.next(); }); }"),
      "java.lang.Exception")

  test("an explicit witness restores the two-effect row: the union cannot be inferred, but it can be written"):
    assertEquals(javac(
      "static int w() throws Log, St { return Fx.<Integer, Log, St>counter2(() -> { Fx.log(\"x\"); Fx.put(1); return Fx.next(); }); }"),
      Nil)

  test("java.util.function does not carry throws: an effect inside forEach does not compile"):
    refused(javac("static void fe(java.util.List<Integer> xs) throws Counter { xs.forEach(x -> Fx.next()); }"), "Counter")

  test("a catch-all 'handles' the effect statically while nothing handles it at run time"):
    assertEquals(javac("static int c() { try { return Fx.next(); } catch (Exception e) { return 0; } }"), Nil)
