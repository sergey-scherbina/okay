package okay.testkit

/** the JVM loads classes late, so an absent optional jar is a question
 * worth asking before use */
private[testkit] object MunitPlatform:
  def missing: Option[String] =
    try { Class.forName("munit.FunSuite"); None }
    catch case _: ClassNotFoundException => Some(
      "okay.testkit.Munit needs org.scalameta:munit, an optional dependency of okay-test: " +
        "add \"org.scalameta\" %%% \"munit\" % \"1.1.1\" % Test — or use okay.diagnose.Diagnostics.around, which needs nothing")
