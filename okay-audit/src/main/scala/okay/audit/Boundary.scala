package okay.audit

enum Layer:
  /** may touch the outside world only through okay effects — scanned, failed on a finding */
  case Business
  /** the trusted base: listed by provider, never failed */
  case Handlers
  /** okay core and its platform: listed as the runtime, never failed */
  case Runtime
  /** no layer declared: reported, neither failed nor listed — the state adoption starts from */
  case Untracked

/** an API outside the boundary and WHY. `api` is one of:
  *  - a package prefix, ending in a dot: `java.net.`
  *  - a class: `java.lang.Thread` (its nested classes with it)
  *  - a member: `java.lang.System#currentTimeMillis`; `#system*` is a name
  *    prefix; `java.util.Date#<init>()` pins the descriptor's start */
final case class Rule(api: String, why: String):
  def matches(r: Ref): Boolean =
    api.indexOf('#') match
      case -1 =>
        if api.endsWith(".") then r.owner.startsWith(api)
        else r.owner == api || r.owner.startsWith(api + "$")
      case h =>
        val owner = api.substring(0, h)
        val rest = api.substring(h + 1)
        val (name, desc) = rest.indexOf('(') match
          case -1 => (rest, "")
          case p => (rest.substring(0, p), rest.substring(p))
        r.name.nonEmpty && r.owner == owner && desc.forall(_ => r.descriptor.startsWith(desc)) &&
          (if name.endsWith("*") then r.name.startsWith(name.dropRight(1)) else r.name == name)

/** a named, reasoned exception for ONE module; an allow nothing matched is reported */
final case class Allow(module: String, api: String, reason: String, owner: String):
  private val rule = Rule(api, reason)
  def matches(module: String, r: Ref): Boolean = this.module == module && rule.matches(r)

/** `layers`: module -> layer. `packages`: module -> package prefix -> layer, for a
  * product that is ONE module by design with business and handlers side by
  * side (okay-watch: `okaywatch.trace` is business, `okaywatch.collect` is
  * handlers, one jar). A class takes the longest matching prefix's layer,
  * else its module's (specs/audit-ready.md, stage 1). */
final case class Boundary(layers: Map[String, Layer], rules: Vector[Rule] = Boundary.Default, allows: Vector[Allow] = Vector.empty,
                          packages: Map[String, Map[String, Layer]] = Map.empty):
  /** the prefix that classifies `cls`, if any: the longest one it is under */
  def prefixOf(module: String, cls: String): Option[String] =
    packages.getOrElse(module, Map.empty).keys.filter(p => cls == p || cls.startsWith(p + ".")).maxByOption(_.length)
  def layerOf(module: String, cls: String): Layer =
    prefixOf(module, cls).flatMap(packages.get(module).flatMap(_.get)).getOrElse(layers.getOrElse(module, Layer.Untracked))

object Boundary:
  /** the three bootstraps every compiler emits — a lambda, a string
    * concatenation, a record's equals/hashCode/toString — are the one
    * hard-coded carve-out, and the report's header names them */
  val Bootstraps: Vector[String] =
    Vector("java.lang.invoke.LambdaMetafactory", "java.lang.invoke.StringConcatFactory", "java.lang.runtime.ObjectMethods")

  /** the types a bootstrap's descriptor names — javac writes them into the
    * pool as bare `CONSTANT_Class` entries (its InnerClasses attribute), so a
    * BARE class reference to one of these is part of the carve-out; a MEMBER
    * reference (`MethodHandles#lookup`) is the escape hatch and stays a finding */
  val BootstrapTypes: Set[String] =
    Set("java.lang.invoke.MethodHandles", "java.lang.invoke.MethodHandles$Lookup", "java.lang.invoke.MethodHandle",
      "java.lang.invoke.MethodType", "java.lang.invoke.CallSite", "java.lang.invoke.TypeDescriptor")

  /** Scala 3's `lazy val` compiles to `MethodHandles.lookup().findVarHandle(...)`
    * and `VarHandle.compareAndSet` in the class's own initializer (measured on
    * the dogfood: every module with a lazy val, 4 references each). A VarHandle
    * reaches MEMORY, not the world, and `lookup()` alone reaches nothing, so
    * these three are carved out; `Lookup#findVirtual`, `#findStatic`,
    * `#unreflect*`, `#defineClass` — the ones that reach code — stay findings */
  val LazyValIdiom: Set[String] =
    Set("java.lang.invoke.MethodHandles#lookup", "java.lang.invoke.MethodHandles$Lookup#findVarHandle")

  def isBootstrap(r: Ref): Boolean =
    Bootstraps.contains(r.owner) || (r.kind == Ref.Kind.Class && BootstrapTypes.contains(r.owner)) ||
      r.owner == "java.lang.invoke.VarHandle" || LazyValIdiom.contains(r.member)

  /** an own `ACC_NATIVE` method: always a finding in a business module — code the JVM cannot see */
  val Native: Rule = Rule("native", "a native method: code neither the JVM nor this check can see")

  private def group(why: String)(apis: String*): Vector[Rule] = apis.toVector.map(Rule(_, why))

  val Default: Vector[Rule] =
    group("network: reached through an effect (Http, Net) whose handler is a Handlers module")(
      "java.net.", "javax.net.", "java.nio.channels.", "java.rmi.", "javax.naming.") ++
    group("files and console: a File or Console effect, journaled, not a direct write")(
      "java.nio.file.", "java.io.File", "java.io.FileInputStream", "java.io.FileOutputStream", "java.io.FileReader",
      "java.io.FileWriter", "java.io.RandomAccessFile", "java.io.Console", "java.lang.System#out", "java.lang.System#err",
      "java.lang.System#in", "scala.Console", "scala.io.Source", "java.util.logging.") ++
    group("databases: a Db effect, so every statement is journaled and replayable")(
      "java.sql.", "javax.sql.") ++
    group("processes and the JVM: the host is the runtime's, not a program's")(
      "java.lang.ProcessBuilder", "java.lang.Process", "java.lang.Runtime#exec", "java.lang.Runtime#exit",
      "java.lang.Runtime#halt", "java.lang.Runtime#addShutdownHook", "java.lang.System#exit", "java.lang.System#load",
      "java.lang.System#loadLibrary", "java.lang.System#getenv", "java.lang.System#getProperty",
      "java.lang.System#setProperty", "scala.sys.process.", "scala.sys.package$#env") ++
    group("time and randomness: nondeterminism breaks replay — a Clock or Random effect instead")(
      "java.lang.System#currentTimeMillis", "java.lang.System#nanoTime", "java.time.Clock#system*",
      "java.time.Instant#now", "java.time.LocalDate#now", "java.time.LocalDateTime#now", "java.time.LocalTime#now",
      "java.time.ZonedDateTime#now", "java.time.OffsetDateTime#now", "java.util.Date#<init>()", "java.util.Random",
      "java.util.concurrent.ThreadLocalRandom", "java.util.SplittableRandom", "java.security.SecureRandom",
      "java.util.UUID#randomUUID", "scala.util.Random") ++
    group("threads: scheduling is the runtime's job (Async), a program forks fibers")(
      "java.lang.Thread", "java.util.concurrent.Executors", "java.util.concurrent.ForkJoinPool",
      "java.util.concurrent.CompletableFuture#runAsync", "java.util.concurrent.CompletableFuture#supplyAsync",
      "scala.concurrent.ExecutionContext$Implicits$") ++
    group("escape hatch: the way around any static check, so the check forbids it")(
      "java.lang.reflect.", "java.lang.invoke.", "java.lang.Class#forName", "java.lang.Class#getMethod",
      "java.lang.Class#getDeclaredMethod", "java.lang.Class#getMethods", "java.lang.Class#getDeclaredMethods",
      "java.lang.Class#getField", "java.lang.Class#getDeclaredField", "java.lang.Class#newInstance",
      "java.lang.Class#getConstructor", "java.lang.Class#getDeclaredConstructor", "java.lang.Class#getConstructors",
      "java.lang.Class#getDeclaredConstructors", "java.lang.ClassLoader", "java.net.URLClassLoader",
      "java.lang.foreign.", "sun.", "jdk.internal.", "java.io.ObjectInputStream", "java.io.ObjectOutputStream")
