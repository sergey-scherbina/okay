package okay.r

import okay.Handler
import okay.codec.Json

/**
 * The subprocess engine (stage 0, specs/r.md): one `Rscript` per
 * session running the shim SHIPPED WITH THIS MODULE (a versioned
 * resource — the shim/host handshake refuses drift loudly). The
 * environment is CLEAN: the parent leaks nothing into R unless config
 * names it.
 *
 * A dead process makes the in-flight call THROW (the supervisor
 * decides — the parallel-resilience fault model); a failing call is a
 * Condition and the process survives.
 */
final class RSubprocess private (proc: Process,
                                 out: java.io.BufferedWriter,
                                 in: java.io.BufferedReader,
                                 val rVersion: String):

  private var nextId = 0

  private def exchange(req: Json): Json =
    nextId += 1
    val id = nextId
    val body = req match
      case Json.JObj(fs) => Json.JObj(("id" -> Json.JNum(id.toDouble)) +: fs)
      case other => other
    out.write(Json.print(body)); out.write("\n"); out.flush()
    val line = in.readLine()
    if line == null then
      throw IllegalStateException(
        "the R process is DEAD (eof on the wire) — a supervisor retry gets a fresh one")
    Json.parse(line)

  private def answer[A](j: Json)(ok: Json => Either[Condition, A]): Either[Condition, A] =
    j match
      case Json.JObj(fs) =>
        val m = fs.toMap
        m.get("condition") match
          case Some(Json.JObj(c)) =>
            val cm = c.toMap
            def str(k: String) = cm.get(k).collect {
              case Json.JStr(s) => s
              // jsonlite unboxes a length-1 character vector to a
              // string, but a longer message stays an array
              case Json.JArr(xs) => xs.collect { case Json.JStr(s) => s }.mkString("\n")
            }.getOrElse("")
            Left(Condition(str("kind"), str("message")))
          case _ => m.get("ok") match
            case Some(v) => ok(v)
            case None => Left(Condition("WireError", s"no ok and no condition in $j"))
      case other => Left(Condition("WireError", s"not an answer: $other"))

  /** the comonadic handler — one operation, one exchange */
  def handler: Handler[REval] = new:
    def handle[A](e: REval[A]): A = e match
      case REval.Call(fn, args) =>
        answer(exchange(Json.JObj(Vector(
          "op" -> Json.JStr("call"), "fn" -> Json.JStr(fn),
          "args" -> Json.JArr(args.map(Wire.enc))))))(v => Right(Wire.dec(v)))
      case REval.Frame(fn, frame, args) =>
        answer(exchange(Json.JObj(Vector(
          "op" -> Json.JStr("frame"), "fn" -> Json.JStr(fn),
          "in" -> Wire.encFrame(frame),
          "args" -> Json.JArr(args.map(Wire.enc))))))(Wire.decFrame)

  /**
   * Presence and version of named packages, mismatches as data naming
   * the package — an analyst's environment drifts, and this turns
   * "wrong forecast, silently" into "loud refusal naming forecast".
   */
  def verify(packages: Map[String, String]): Vector[String] =
    val asked = Json.JArr(packages.keys.toVector.sorted.map(Json.JStr(_)))
    answer(exchange(Json.JObj(Vector(
      "op" -> Json.JStr("verify"), "packages" -> asked))))(v => Right(v)) match
      case Left(c) => Vector(s"verify itself failed: ${c.kind}: ${c.message}")
      case Right(Json.JObj(fs)) =>
        val have = fs.toMap.get("packages") match
          case Some(Json.JObj(ps)) => ps.toMap
          case _ => Map.empty[String, Json]
        packages.toVector.sortBy(_._1).flatMap { (name, want) =>
          version(have.get(name)) match
            case None => Some(s"package '$name' is MISSING (wanted $want)")
            case Some(v) if !v.startsWith(want) => Some(s"package '$name' is $v, wanted $want")
            case _ => None
        }
      case Right(other) => Vector(s"verify answered strangely: $other")

  /** jsonlite unboxes a length-1 character vector; an absent package
   * is a null the encoder may drop entirely */
  private def version(j: Option[Json]): Option[String] = j match
    case Some(Json.JStr(v)) => Some(v)
    case Some(Json.JArr(Vector(Json.JStr(v)))) => Some(v)
    case _ => None

  def close(): Unit =
    try { out.close(); in.close() } catch case _: Exception => ()
    proc.destroy()

object RSubprocess:

  val ShimVersion = 1

  /**
   * Start a session: the configured `Rscript` (resolved against PATH
   * when relative — the child's env is empty, so resolution happens
   * HERE), the shim from this jar, a CLEAN environment plus exactly
   * what `env` names.
   */
  def start(rscript: String = "Rscript",
            env: Map[String, String] = Map.empty): RSubprocess =
    val shim = java.nio.file.Files.createTempFile("okay-r-shim", ".R")
    val res = getClass.getResourceAsStream("/okay/r/shim.R")
    if res == null then throw IllegalStateException("the shim resource is missing from the jar")
    try java.nio.file.Files.copy(res, shim, java.nio.file.StandardCopyOption.REPLACE_EXISTING)
    finally res.close()
    shim.toFile.deleteOnExit()
    startWith(rscript, shim, env)

  /** the seam the handshake test uses: any shim file */
  private[r] def startWith(rscript: String, shim: java.nio.file.Path,
                           env: Map[String, String]): RSubprocess =
    val exe = resolve(rscript)
    // --vanilla: no site file, no profile, no saved workspace — the
    // clean-environment rule extended to R's OWN startup, which reads
    // four files by default and would otherwise import an analyst's
    // options into every call
    val pb = ProcessBuilder(exe, "--vanilla", shim.toString)
    pb.environment().clear()
    env.foreach((k, v) => pb.environment().put(k, v))
    pb.redirectErrorStream(false)
    val proc =
      try pb.start()
      catch case e: java.io.IOException =>
        throw IllegalStateException(
          s"'$rscript' did not start: ${e.getMessage} — the wrong-environment refusal, at its loudest")
    val out = java.io.BufferedWriter(java.io.OutputStreamWriter(proc.getOutputStream, "UTF-8"))
    val in = java.io.BufferedReader(java.io.InputStreamReader(proc.getInputStream, "UTF-8"))

    // the handshake: the shim speaks first, and drift refuses loudly
    val hello = in.readLine()
    if hello == null then
      throw IllegalStateException(s"'$rscript' started but the shim answered nothing (stderr may know)")
    val fields = Json.parse(hello) match
      case Json.JObj(fs) => fs.toMap
      case _ => Map.empty[String, Json]
    def one(k: String): Option[String] = fields.get(k).collect {
      case Json.JStr(s) => s
      case Json.JArr(Vector(Json.JStr(s))) => s
    }
    // the named prerequisite, refused by name rather than as a stack
    // trace from inside a shim
    one("fatal").foreach { why =>
      proc.destroy()
      throw IllegalStateException(why)
    }
    val shimV = fields.get("shim").collect {
      case Json.JNum(n) => n.toInt
      case Json.JArr(Vector(Json.JNum(n))) => n.toInt
    }.getOrElse(-1)
    if shimV != ShimVersion then
      proc.destroy()
      throw IllegalStateException(
        s"shim/host version drift: the shim says v$shimV, this host speaks v$ShimVersion — refuse rather than guess")
    new RSubprocess(proc, out, in, one("r").getOrElse("?"))

  private def resolve(rscript: String): String =
    if rscript.contains("/") then rscript
    else
      sys.env.getOrElse("PATH", "").split(":").iterator
        .map(d => java.nio.file.Paths.get(d, rscript))
        .find(p => java.nio.file.Files.isExecutable(p))
        .map(_.toString)
        .getOrElse(rscript)   // let start() produce the loud refusal
