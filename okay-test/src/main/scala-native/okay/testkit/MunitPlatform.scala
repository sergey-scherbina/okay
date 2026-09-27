package okay.testkit

/** a linked program has every class it uses, or it did not link: there is
 * no absent jar to report at run time (Class.forName does not exist here) */
private[testkit] object MunitPlatform:
  def missing: Option[String] = None
