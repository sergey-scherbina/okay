package okay

/**
 * Which cont-stack road a JMH fork runs (cont-stack-jmh-native-access,
 * specs/cont-stack.md): EXACT when the `jdk22/` StackRoom reads the
 * stack — JDK 22+, the packaged jar on the classpath (build.sbt), and
 * `-jvmArgsAppend --enable-native-access=ALL-UNNAMED` — and COUNTED
 * otherwise. Printed once per fork from a trial-level setup, outside
 * every measurement, so a lane's log says which road its number is.
 */
object ContStackRoad:
  def announce(): Unit =
    val road = if StackRoom.sp() >= 0 then "exact (the stack is read through FFM)" else "counted (the stack is not readable)"
    // where the reader came from: a classes directory, or a jar without
    // `Multi-Release`, is never versioned (build.sbt), and that was the
    // whole defect this lane found
    val from = Option(classOf[StackRoom.type].getProtectionDomain.getCodeSource)
      .map(c => java.io.File(c.getLocation.getPath).getName).getOrElse("?")
    println(s"# cont-stack road: $road; StackRoom from $from")
