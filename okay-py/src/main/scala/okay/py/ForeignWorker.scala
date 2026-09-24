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
final class ForeignWorker private (link: WireLink, val pythonVersion: String):

  private var nextId = 0

  private def exchange(req: Json): Json =
    nextId += 1
    val id = nextId
    val body = req match
      case Json.JObj(fs) => Json.JObj(("id" -> Json.JNum(id.toDouble)) +: fs)
      case other => other
    send(body)

  /** one message out, the next one in — `exchange` without an id, which
   * is how a `resume` goes: it answers an ask, it opens nothing */
  private def send(body: Json): Json =
    link.roundTrip(Json.print(body)) match
      case Some(line) => ForeignWorker.whole(line)
      case None =>
        throw IllegalStateException("the worker is DEAD (eof on the wire) — a supervisor retry gets a fresh one")

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
  def handler: Handler[ForeignEval] = new:
    def handle[A](e: ForeignEval[A]): A = e match
      case ForeignEval.Call(fn, args) =>
        answer(exchange(Json.JObj(Vector(
          "op" -> Json.JStr("call"), "fn" -> Json.JStr(fn),
          "args" -> Json.JArr(args.map(Wire.enc))))))(v => Right(Wire.dec(v)))
      case ForeignEval.Frame(fn, frame, args) =>
        answer(exchange(Json.JObj(Vector(
          "op" -> Json.JStr("frame"), "fn" -> Json.JStr(fn),
          "in" -> Wire.encFrame(frame),
          "args" -> Json.JArr(args.map(Wire.enc))))))(Wire.decFrame)
      case ForeignEval.Start(fn, args, cbs) =>
        stepOf(exchange(Json.JObj(Vector(
          "op" -> Json.JStr("start"), "fn" -> Json.JStr(fn),
          "args" -> Json.JArr(args.map(Wire.enc)),
          "callbacks" -> Json.JArr(cbs.map(Json.JStr(_)))))))
      case ForeignEval.Resume(k, a) =>
        val answered = a match
          case Right(v) => "ok" -> Wire.enc(v)
          case Left(c) => "condition" -> Json.JObj(Vector(
            "kind" -> Json.JStr(c.kind), "message" -> Json.JStr(c.message)))
        stepOf(send(Json.JObj(Vector(
          "op" -> Json.JStr("resume"), "k" -> Json.JNum(k.toDouble), answered))))
      case ForeignEval.Hold(fn, args) =>
        answer(exchange(Json.JObj(Vector(
          "op" -> Json.JStr("hold"), "fn" -> Json.JStr(fn),
          "args" -> Json.JArr(args.map(Wire.enc))))))(v => Wire.asRef(Wire.dec(v)))
      case ForeignEval.Method(r, name, args, h) =>
        answer(exchange(Json.JObj(Vector(
          "op" -> Json.JStr("method"), "ref" -> Json.JNum(r.id.toDouble), "name" -> Json.JStr(name),
          "args" -> Json.JArr(args.map(Wire.enc)), "hold" -> Json.JBool(h)))))(v => Right(Wire.dec(v)))
      case ForeignEval.Attr(r, name) =>
        answer(exchange(Json.JObj(Vector(
          "op" -> Json.JStr("attr"), "ref" -> Json.JNum(r.id.toDouble),
          "name" -> Json.JStr(name)))))(v => Right(Wire.dec(v)))
      case ForeignEval.Program(run, fn, args) =>
        answer(exchange(Json.JObj(Vector(
          "op" -> Json.JStr("program"), "run" -> Json.JNum(run.toDouble), "fn" -> Json.JStr(fn),
          "args" -> Json.JArr(args.map(Wire.enc))))))(Wire.decNode)
      case ForeignEval.Continue(run, k, a) =>
        answer(exchange(Json.JObj(Vector(
          "op" -> Json.JStr("continue"), "run" -> Json.JNum(run.toDouble), "k" -> Json.JNum(k.toDouble),
          "answer" -> Wire.enc(a)))))(Wire.decNode)
      case ForeignEval.Forget(run) =>
        val _ = exchange(Json.JObj(Vector("op" -> Json.JStr("forget"), "run" -> Json.JNum(run.toDouble))))
      case ForeignEval.Release(r) =>
        // idempotent on both sides: releasing twice, or a ref the
        // process never held, is not an error worth a program's attention
        val _ = exchange(Json.JObj(Vector("op" -> Json.JStr("release"), "ref" -> Json.JNum(r.id.toDouble))))

  /** a call's next message: an ask, or its answer */
  private def stepOf(j: Json): PyStep =
    Wire.step(j).getOrElse(PyStep.Done(answer(j)(v => Right(Wire.dec(v)))))

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

  def close(): Unit = link.close()

object ForeignWorker:

  val ShimVersion = 6

  /**
   * Start a worker: the configured interpreter (resolved against
   * PATH when relative — the child's env is empty, so resolution
   * happens HERE), the shim from this jar, a CLEAN environment plus
   * exactly what `env` names.
   */
  def start(python: String = "python3",
            env: Map[String, String] = Map.empty,
            /** inline modules to ship on the worker's path (foreign-inline-modules) */
            modules: Seq[PyModule] = Nil): ForeignWorker =
    startIn(python, PyModule.env(modules, env))

  /** `start` once the modules are already in the environment */
  private[py] def startIn(python: String, env: Map[String, String]): ForeignWorker =
    val shim = java.nio.file.Files.createTempFile("okay-py-shim", ".py")
    val res = getClass.getResourceAsStream("/okay/py/shim.py")
    if res == null then throw IllegalStateException("the shim resource is missing from the jar")
    try java.nio.file.Files.copy(res, shim, java.nio.file.StandardCopyOption.REPLACE_EXISTING)
    finally res.close()
    shim.toFile.deleteOnExit()
    startWith(python, shim, env)

  /** the seam the handshake test uses: any shim file */
  private[py] def startWith(python: String, shim: java.nio.file.Path,
                            env: Map[String, String]): ForeignWorker =
    startCommand(Vector(resolve(python), shim.toString), python, env)

  /**
   * An engine over ANY process that speaks the okay wire — the handshake
   * line first, then one JSON request and one answer per line
   * (remote-foreign). A compiled Haskell worker built on the `Okay` module
   * this jar ships (`/okay/hs/Okay.hs`) is one: its programs-as-data run
   * through `Py.program` exactly as Python's do, multi-shot included.
   */
  def speaking(command: Seq[String], env: Map[String, String] = Map.empty): ForeignWorker =
    startCommand(command.toVector, command.headOption.getOrElse("?"), env)

  /**
   * The engine over ANY link (polyglot-one-wire): the far side speaks the
   * handshake first, then one request and one answer per line. Pipes,
   * a socket, an in-process call — the same `Py.program`, the same
   * callbacks and multi-shot, the same `Durable`.
   */
  def over(link: WireLink, name: String = "the worker"): ForeignWorker =
    val hello = link.hello().getOrElse {
      link.close()
      throw IllegalStateException(s"$name answered nothing (stderr may know)")
    }
    val (shimV, pyV) = whole(hello) match
      case Json.JObj(fs) =>
        val m = fs.toMap
        (m.get("shim").collect { case Json.JNum(n) => n.toInt }.getOrElse(-1),
          m.get("python").collect { case Json.JStr(s) => s }.getOrElse("?"))
      case _ => (-1, "?")
    if shimV != ShimVersion then
      link.close()
      throw IllegalStateException(
        s"shim/host version drift: $name says v$shimV, this host speaks v$ShimVersion — refuse rather than guess")
    new ForeignWorker(link, pyV)

  /**
   * A wire line, read STRICTLY. `Json.parse` is total: it repairs damaged
   * text and answers what it could read, which is right for a document a
   * person wrote and wrong for a protocol line — a reply cut short (a
   * truncated FFM read, a dropped TCP tail) must not be read as a smaller
   * reply. Found by in-process-worker's mutant: an answer one byte short
   * passed every test. The fast strict parser first; where it declines,
   * the total one, refused if it had to repair anything.
   */
  private[py] def whole(line: String): Json =
    if !balanced(line) then
      throw IllegalStateException(s"the worker's line is not whole JSON (cut short?): ${line.take(200)}")
    okay.codec.JsonValue.parse(line).getOrElse {
      val j = Json.parse(line)
      def damaged(x: Json): Boolean = x match
        case Json.JErr(_) => true
        case Json.JArr(vs) => vs.exists(damaged)
        case Json.JObj(fs) => fs.exists((_, v) => damaged(v))
        case _ => false
      if damaged(j) || !line.trim.endsWith("}") then
        throw IllegalStateException(s"the worker's line is not whole JSON (cut short?): ${line.take(200)}")
      j
    }

  /** every bracket outside a string closed, exactly at the end: a line cut
   * after its inner `}` still parses as a smaller object, and this is the
   * one check such a cut cannot pass */
  private def balanced(line: String): Boolean =
    var depth = 0
    var inString = false
    var escaped = false
    var closedAt = -1
    var i = 0
    val t = line.trim
    while i < t.length do
      val c = t.charAt(i)
      if inString then
        if escaped then escaped = false
        else if c == '\\' then escaped = true
        else if c == '"' then inString = false
      else c match
        case '"' => inString = true
        case '{' | '[' => depth += 1
        case '}' | ']' =>
          depth -= 1
          if depth == 0 then closedAt = i
        case _ => ()
      i += 1
    !inString && depth == 0 && closedAt == t.length - 1

  /** a worker SERVING the okay wire on TCP (`okay::serve_tcp`, `okay.ServeTCP`):
   * another process, or another machine — plain TCP, see `WireLink.tcp` */
  def connect(host: String, port: Int): ForeignWorker =
    over(WireLink.tcp(host, port), s"the worker at $host:$port")

  private def startCommand(cmd: Vector[String], python: String, env: Map[String, String]): ForeignWorker =
    val pb = ProcessBuilder(cmd*)
    pb.environment().clear()             // the clean-env rule: nothing leaks
    env.foreach((k, v) => pb.environment().put(k, v))
    pb.redirectErrorStream(false)
    val proc =
      try pb.start()
      catch case e: java.io.IOException =>
        throw IllegalStateException(s"the interpreter '$python' did not start: ${e.getMessage} — the wrong-venv refusal, at its loudest")
    // the handshake: the shim speaks first, and drift refuses loudly
    over(WireLink.pipes(proc), s"'$python'")

  private def resolve(python: String): String =
    if python.contains("/") then python
    else
      sys.env.getOrElse("PATH", "").split(":").iterator
        .map(d => java.nio.file.Paths.get(d, python))
        .find(p => java.nio.file.Files.isExecutable(p))
        .map(_.toString)
        .getOrElse(python)   // let start() produce the loud refusal
