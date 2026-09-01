package okay.py

import okay.Handler
import okay.codec.Json

/**
 * The subprocess engine (stage 0): one `python3` per session running
 * the shim SHIPPED WITH THIS MODULE (a versioned resource — the
 * shim/host handshake refuses drift loudly). The environment is
 * CLEAN: the parent leaks nothing into Python unless config names
 * it. Blocking on the pipe — a virtual thread parks there, the
 * Provider.openAi trade, stated not hidden.
 *
 * A dead process makes the in-flight call THROW (the supervisor
 * decides — the parallel-resilience fault model); a failing call is
 * a Condition and the worker survives.
 */
final class PySubprocess private (proc: Process,
                                  out: java.io.BufferedWriter,
                                  in: java.io.BufferedReader,
                                  val pythonVersion: String):

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
      throw IllegalStateException("the python worker is DEAD (eof on the wire) — a supervisor retry gets a fresh process")
    Json.parse(line)

  private def answer[A](j: Json)(ok: Json => Either[Condition, A]): Either[Condition, A] =
    j match
      case Json.JObj(fs) =>
        val m = fs.toMap
        m.get("condition") match
          case Some(Json.JObj(c)) =>
            val cm = c.toMap
            def str(k: String) = cm.get(k).collect { case Json.JStr(s) => s }.getOrElse("")
            Left(Condition(str("kind"), str("message")))
          case _ => m.get("ok") match
            case Some(v) => ok(v)
            case None => Left(Condition("WireError", s"no ok and no condition in $j"))
      case other => Left(Condition("WireError", s"not an answer: $other"))

  /** the comonadic handler — one operation, one exchange */
  def handler: Handler[PyEval] = new:
    def handle[A](e: PyEval[A]): A = e match
      case PyEval.Call(fn, args) =>
        answer(exchange(Json.JObj(Vector(
          "op" -> Json.JStr("call"), "fn" -> Json.JStr(fn),
          "args" -> Json.JArr(args.map(Wire.enc))))))(v => Right(Wire.dec(v)))
      case PyEval.Frame(fn, frame, args) =>
        answer(exchange(Json.JObj(Vector(
          "op" -> Json.JStr("frame"), "fn" -> Json.JStr(fn),
          "in" -> Wire.encFrame(frame),
          "args" -> Json.JArr(args.map(Wire.enc))))))(Wire.decFrame)

  /** presence and version of named packages via importlib.metadata,
   * mismatches as data naming the package — the wrong venv becomes
   * a loud startup refusal instead of a subtly different model fit */
  def verify(packages: Map[String, String]): Vector[String] =
    val asked = Json.JArr(packages.keys.toVector.sorted.map(Json.JStr(_)))
    answer(exchange(Json.JObj(Vector(
      "op" -> Json.JStr("verify"), "packages" -> asked))))(v => Right(v)) match
      case Left(c) => Vector(s"verify itself failed: ${c.kind}: ${c.message}")
      case Right(Json.JObj(fs)) =>
        val m = fs.toMap
        val have = m.get("packages") match
          case Some(Json.JObj(ps)) => ps.toMap
          case _ => Map.empty[String, Json]
        packages.toVector.sortBy(_._1).flatMap { (name, want) =>
          have.get(name) match
            case Some(Json.JNull) | None => Some(s"package '$name' is MISSING (wanted $want)")
            case Some(Json.JStr(v)) if !v.startsWith(want) =>
              Some(s"package '$name' is $v, wanted $want")
            case _ => None
        }
      case Right(other) => Vector(s"verify answered strangely: $other")

  def close(): Unit =
    try { out.close(); in.close() } catch case _: Exception => ()
    proc.destroy()

object PySubprocess:

  val ShimVersion = 1

  /**
   * Start a worker: the configured interpreter (resolved against
   * PATH when relative — the child's env is empty, so resolution
   * happens HERE), the shim from this jar, a CLEAN environment plus
   * exactly what `env` names.
   */
  def start(python: String = "python3",
            env: Map[String, String] = Map.empty): PySubprocess =
    val shim = java.nio.file.Files.createTempFile("okay-py-shim", ".py")
    val res = getClass.getResourceAsStream("/okay/py/shim.py")
    if res == null then throw IllegalStateException("the shim resource is missing from the jar")
    try java.nio.file.Files.copy(res, shim, java.nio.file.StandardCopyOption.REPLACE_EXISTING)
    finally res.close()
    shim.toFile.deleteOnExit()
    startWith(python, shim, env)

  /** the seam the handshake test uses: any shim file */
  private[py] def startWith(python: String, shim: java.nio.file.Path,
                            env: Map[String, String]): PySubprocess =
    val exe = resolve(python)
    val pb = ProcessBuilder(exe, shim.toString)
    pb.environment().clear()             // the clean-env rule: nothing leaks
    env.foreach((k, v) => pb.environment().put(k, v))
    pb.redirectErrorStream(false)
    val proc =
      try pb.start()
      catch case e: java.io.IOException =>
        throw IllegalStateException(s"the interpreter '$python' did not start: ${e.getMessage} — the wrong-venv refusal, at its loudest")
    val out = java.io.BufferedWriter(java.io.OutputStreamWriter(proc.getOutputStream, "UTF-8"))
    val in = java.io.BufferedReader(java.io.InputStreamReader(proc.getInputStream, "UTF-8"))

    // the handshake: the shim speaks first, and drift refuses loudly
    val hello = in.readLine()
    if hello == null then
      throw IllegalStateException(s"'$python' started but the shim answered nothing (stderr may know)")
    val (shimV, pyV) = Json.parse(hello) match
      case Json.JObj(fs) =>
        val m = fs.toMap
        (m.get("shim").collect { case Json.JNum(n) => n.toInt }.getOrElse(-1),
          m.get("python").collect { case Json.JStr(s) => s }.getOrElse("?"))
      case _ => (-1, "?")
    if shimV != ShimVersion then
      proc.destroy()
      throw IllegalStateException(
        s"shim/host version drift: the shim says v$shimV, this host speaks v$ShimVersion — refuse rather than guess")
    new PySubprocess(proc, out, in, pyV)

  private def resolve(python: String): String =
    if python.contains("/") then python
    else
      sys.env.getOrElse("PATH", "").split(":").iterator
        .map(d => java.nio.file.Paths.get(d, python))
        .find(p => java.nio.file.Files.isExecutable(p))
        .map(_.toString)
        .getOrElse(python)   // let start() produce the loud refusal
