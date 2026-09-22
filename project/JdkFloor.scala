import sbt._
import sbt.Keys._

/**
 * TEST code carries no JDK floor (java-gatherers, 2026-09-23).
 *
 * build.sbt puts `-java-output-version 17` on every Scala 3 module, so
 * the compiler refuses a main-code API past the module's floor now
 * that sbt runs on JDK 25. Tests are the other case, and this undoes
 * the flag for them, in every project (and in Jmh, which extends Test):
 *
 *  - they run on JDK 26 by default (Test / javaHome), and a floor is a
 *    promise about what USERS run;
 *  - forty-odd test files call JDK 21 API unconditionally (virtual
 *    threads, `Thread.ofPlatform`) — so `jdkFloor(21)` would be the
 *    honest floor for them, and it would make every test class of the
 *    module major 65, so `verifyJdk17` could not even load the suites
 *    that pass on 17 today. Unflagged, test bytecode stays 61, which
 *    is exactly what it was when sbt ran on 21.
 *
 * What is given up: a test calling a 22+ API compiles. `verifyJdk17`
 * still finds it, at run time, which is where a test's JDK matters.
 */
object JdkFloor extends AutoPlugin {
  override def trigger = allRequirements

  /** the flag and its value, removed wherever they appear */
  def unflagged(opts: Seq[String]): Seq[String] = {
    val i = opts.indexOf("-java-output-version")
    if (i < 0) opts else opts.patch(i, Nil, 2)
  }

  override def projectSettings = Seq(Test / scalacOptions ~= unflagged)
}
