package okay.codec

/**
 * The JVM's way to reach `okay-staging` WITHOUT depending on it
 * (staging-seam): a module that crosses to JS and Native cannot name
 * the compiler, but a JVM program built from it can still carry the
 * staging module on its classpath. `autoInstall()` looks for
 * `okay.staging.RuntimeStaged` by name and, if it is there and not
 * switched off, calls its `install()` — from then on every
 * `Codecs.json/cbor` door in the process answers the staged codec.
 * Absent, off, or failing: nothing happens and the doors stay the
 * interpreter, and the answer says which.
 */
object Staging {

  enum Outcome:
    case Installed
    /** the module is not on the classpath */
    case Absent
    /** `-Dokay.staging=off` or a failing install; the interpreter stays */
    case Refused(why: String)

  /** the last answer, for a boot log */
  @volatile var last: Option[Outcome] = None

  def autoInstall(): Outcome =
    val out =
      try
        val cls = Class.forName("okay.staging.RuntimeStaged$")
        val module = cls.getField("MODULE$").get(null)
        val ok = cls.getMethod("install").invoke(module)
        if ok == true then Outcome.Installed
        else Outcome.Refused("switched off (okay.staging=off)")
      catch
        case _: ClassNotFoundException => Outcome.Absent
        case e: Throwable => Outcome.Refused(e.toString)
    last = Some(out)
    out
}
