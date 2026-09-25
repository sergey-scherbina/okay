package okay.kernel

import java.nio.file.{Files, Path}
import java.util.ServiceLoader
import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

/**
 * Finding plugins on the JVM (specs/kernel.md): every provider named in
 * `META-INF/services/okay.kernel.Plugin`. One that cannot be loaded or
 * constructed is a `LoadFailed` and the rest still load — a broken jar
 * in a plugins directory is a sentence on the start page, not a dead
 * program.
 */
object Discover:

  def services(loader: ClassLoader = classOf[Plugin].getClassLoader)
  : (Vector[Plugin], Vector[Problem.LoadFailed]) =
    val found = Vector.newBuilder[Plugin]
    val failed = Vector.newBuilder[Problem.LoadFailed]
    // the stream form: each provider's CLASS is found first and its
    // construction is a separate step, so one bad provider is one
    // failure rather than the end of the iteration
    val it = ServiceLoader.load(classOf[Plugin], loader).stream().iterator()
    var going = true
    while going do
      try
        if it.hasNext then
          val provider = it.next()
          try found += provider.get()
          catch case NonFatal(e) => failed += Problem.LoadFailed(provider.`type`.getName, why(e))
        else going = false
      catch
        case e: java.util.ServiceConfigurationError =>
          failed += Problem.LoadFailed("a service entry", why(e))
    (found.result(), failed.result())

  /** every `*.jar` in `dir`, under one class loader whose parent is the
   * host's: the plugins directory an Enterprise box is given. A missing
   * directory is no plugins, not a failure.
   *
   * The service entries are read from THOSE JARS, not through
   * `ServiceLoader`: a loader whose parent is the host's would also
   * answer the host's own `META-INF/services`, and every built-in plugin
   * would come back a second time as a duplicate. A class an entry names
   * may live in the jar or in the host. */
  def jars(dir: Path, parent: ClassLoader = classOf[Plugin].getClassLoader)
  : (Vector[Plugin], Vector[Problem.LoadFailed]) =
    if !Files.isDirectory(dir) then (Vector.empty, Vector.empty)
    else
      val list = Files.list(dir)
      val jars =
        try list.iterator.asScala.filter(_.toString.endsWith(".jar")).toVector.sortBy(_.toString)
        finally list.close()
      if jars.isEmpty then (Vector.empty, Vector.empty)
      else
        val loader = java.net.URLClassLoader(jars.map(_.toUri.toURL).toArray, parent)
        val found = Vector.newBuilder[Plugin]
        val failed = Vector.newBuilder[Problem.LoadFailed]
        for jar <- jars; name <- entries(jar, failed) do
          try Class.forName(name, true, loader).getDeclaredConstructor().newInstance() match
            case p: Plugin => found += p
            case other => failed += Problem.LoadFailed(name, s"not an okay.kernel.Plugin (${other.getClass.getName})")
          catch case NonFatal(e) => failed += Problem.LoadFailed(name, why(e))
        (found.result(), failed.result())

  /** the class names one jar's service file lists: `#` starts a comment */
  private def entries(jar: Path, failed: scala.collection.mutable.Builder[Problem.LoadFailed, ?]): Vector[String] =
    try
      val jf = java.util.jar.JarFile(jar.toFile)
      try
        Option(jf.getEntry("META-INF/services/okay.kernel.Plugin")).fold(Vector.empty[String]) { e =>
          val text = String(jf.getInputStream(e).readAllBytes(), java.nio.charset.StandardCharsets.UTF_8)
          text.linesIterator.map(_.takeWhile(_ != '#').trim).filter(_.nonEmpty).toVector
        }
      finally jf.close()
    catch case NonFatal(e) =>
      failed += Problem.LoadFailed(jar.getFileName.toString, why(e))
      Vector.empty

  private def why(e: Throwable): String =
    val root = Iterator.iterate(e)(_.getCause).takeWhile(_ != null).toVector.last
    Option(root.getMessage).filter(_.nonEmpty).fold(root.getClass.getName)(m => s"${root.getClass.getSimpleName}: $m")
