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
 *
 * The wire itself — handshake, negotiation, deadline, the Arrow road,
 * death — is the language-neutral `WireSession` (foreign-one-r), which R
 * shares; this class is the handler that speaks `PyValue` over it.
 */
final class ForeignWorker private (session: WireSession):

  /** the far side's own version, from its hello */
  val pythonVersion: String = session.version

  /** false once the wire is gone: an end of stream, or a deadline that
   * closed it. A supervisor reads this to know it must reopen. */
  def alive: Boolean = session.alive

  /** what the handshake settled on: "json/none" (the plain JSON lines),
   * "json/deflate", "cbor/none", "cbor/deflate" */
  def wire: String = session.wire

  private def arrow = session.arrow
  private def exchange(req: Json): Json = session.exchange(req)
  private def send(body: Json): Json = session.send(body)

  /** frames that crossed as Arrow: (sent, answered) — the JSON road
   * gives the same values, so this is how a caller (or a test) sees
   * which road a frame took */
  def arrowFrames: (Long, Long) = session.arrowFrames

  /**
   * A TABLE through `fn` (facade-frame-seam): where this worker speaks
   * Arrow the Table goes to the wire as itself and the answer comes back
   * as the Table it read — no PyFrame in between; where it does not, the
   * Table is converted once each way. What `ForeignEval.Frame` does for a
   * PyFrame, for a caller whose frame is already a Table.
   */
  def frameTable(fn: String, table: okay.arrow.Table, args: Vector[PyValue]): Either[Condition, okay.arrow.Table] = timed:
    val head = Vector("op" -> Json.JStr("frame"), "fn" -> Json.JStr(fn), "args" -> Json.JArr(args.map(Wire.enc)))
    def asTable(f: PyFrame): Either[Condition, okay.arrow.Table] = ArrowFrames.table(f).left.map(Condition("Frame", _))
    if arrow then
      val (j, got) = session.exchangeArrow(head, table)
      answer(j)(v => got.fold(Wire.decFrame(v).flatMap(asTable))(Right(_)))
    else
      val sent = try Right(ArrowFrames.frame(table))
        catch case e: IllegalStateException => Left(Condition("Frame", Option(e.getMessage).getOrElse("")))
      sent.flatMap(f => answer(exchange(Json.JObj(head :+ ("in" -> Wire.encFrame(f)))))(Wire.decFrame).flatMap(asTable))

  /** a timeout is DATA for an operation that answers an Either: the call
   * failed, the program can see it and decide (stage 6) */
  private def timed[A](f: => Either[Condition, A]): Either[Condition, A] =
    try f catch case t: ForeignWorker.TimedOut => Left(Condition("timeout", t.getMessage))

  private def timedStep(f: => PyStep): PyStep =
    try f catch case t: ForeignWorker.TimedOut => PyStep.Done(Left(Condition("timeout", t.getMessage)))

  private def answer[A](j: Json)(ok: Json => Either[Condition, A]): Either[Condition, A] =
    WireSession.answer(j)(Condition(_, _))(ok)

  /** the comonadic handler — one operation, one exchange */
  def handler: Handler[ForeignEval] = new:
    def handle[A](e: ForeignEval[A]): A = e match
      case ForeignEval.Call(fn, args) => timed:
        answer(exchange(Json.JObj(Vector(
          "op" -> Json.JStr("call"), "fn" -> Json.JStr(fn),
          "args" -> Json.JArr(args.map(Wire.enc))))))(v => Right(Wire.dec(v)))
      case ForeignEval.Frame(fn, frame, args) => timed:
        val head = Vector("op" -> Json.JStr("frame"), "fn" -> Json.JStr(fn), "args" -> Json.JArr(args.map(Wire.enc)))
        val table = if arrow then ArrowFrames.table(frame) else Left("")
        table match
          case Right(t) =>
            val (j, got) = session.exchangeArrow(head, t)
            answer(j)(v => got.fold(Wire.decFrame(v))(t => Right(ArrowFrames.frame(t))))
          case Left(why) if arrow && session.frames.strict =>
            Left(Condition("NotArrow", s"this host's given FrameFormat is arrow, and $why"))
          case Left(_) =>
            answer(exchange(Json.JObj(head :+ ("in" -> Wire.encFrame(frame)))))(Wire.decFrame)
      case ForeignEval.Start(fn, args, cbs) => timedStep:
        stepOf(exchange(Json.JObj(Vector(
          "op" -> Json.JStr("start"), "fn" -> Json.JStr(fn),
          "args" -> Json.JArr(args.map(Wire.enc)),
          "callbacks" -> Json.JArr(cbs.map(Json.JStr(_)))))))
      case ForeignEval.Resume(k, a) =>
        val answered = a match
          case Right(v) => "ok" -> Wire.enc(v)
          case Left(c) => "condition" -> Json.JObj(Vector(
            "kind" -> Json.JStr(c.kind), "message" -> Json.JStr(c.message)))
        timedStep(stepOf(send(Json.JObj(Vector(
          "op" -> Json.JStr("resume"), "k" -> Json.JNum(k.toDouble), answered)))))
      case ForeignEval.Hold(fn, args) => timed:
        answer(exchange(Json.JObj(Vector(
          "op" -> Json.JStr("hold"), "fn" -> Json.JStr(fn),
          "args" -> Json.JArr(args.map(Wire.enc))))))(v => Wire.asRef(Wire.dec(v)))
      case ForeignEval.Method(r, name, args, h) => timed:
        answer(exchange(Json.JObj(Vector(
          "op" -> Json.JStr("method"), "ref" -> Json.JNum(r.id.toDouble), "name" -> Json.JStr(name),
          "args" -> Json.JArr(args.map(Wire.enc)), "hold" -> Json.JBool(h)))))(v => Right(Wire.dec(v)))
      case ForeignEval.Attr(r, name) => timed:
        answer(exchange(Json.JObj(Vector(
          "op" -> Json.JStr("attr"), "ref" -> Json.JNum(r.id.toDouble),
          "name" -> Json.JStr(name)))))(v => Right(Wire.dec(v)))
      case ForeignEval.Program(run, fn, args) => timed:
        answer(exchange(Json.JObj(Vector(
          "op" -> Json.JStr("program"), "run" -> Json.JNum(run.toDouble), "fn" -> Json.JStr(fn),
          "args" -> Json.JArr(args.map(Wire.enc))))))(Wire.decNode)
      case ForeignEval.Continue(run, k, a) => timed:
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
  def verify(packages: Map[String, String]): Vector[String] = session.verify(packages)

  def close(): Unit = session.close()

object ForeignWorker:

  /** an answer that did not come within the deadline (`WireSession`'s) */
  type TimedOut = WireSession.TimedOut

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
            modules: Seq[PyModule] = Nil)(using WireFormat, WireCompression, WireDeadline)(using okay.codec.FrameFormat): ForeignWorker =
    startIn(python, PyModule.env(modules, env))

  /** `start`, with the wire picked by an explicit `okay.codec.WireChoice`
   * instead of the givens above — for a caller that decides the wire at
   * RUNTIME, from a string (a flag, a config file): `WireChoice.named(format
   * = cfg.format, frames = cfg.frames)`, or `WireChoice.default` for what
   * `start` does with no import */
  def startWithWire(wire: okay.codec.WireChoice, python: String = "python3",
                    env: Map[String, String] = Map.empty, modules: Seq[PyModule] = Nil): ForeignWorker =
    startIn(python, PyModule.env(modules, env))(using wire.format, wire.compression, wire.deadline)(using wire.frames)

  /** `start` once the modules are already in the environment */
  private[py] def startIn(python: String, env: Map[String, String])(using WireFormat, WireCompression, WireDeadline)(using okay.codec.FrameFormat): ForeignWorker =
    startWith(python, shimFile(), env)

  /** the shim from this jar, as a file a process can run */
  private def shimFile(): java.nio.file.Path =
    WireSession.shipped(classOf[ForeignWorker], "/okay/py/shim.py", "okay-py-shim", ".py")

  /** the Python worker as a COMMAND, for a process okay does not start
   * itself: `ForeignGateway` runs one per connection (stage 7) */
  def pythonCommand(python: String = "python3", modules: Seq[PyModule] = Nil,
                    env: Map[String, String] = Map.empty): WorkerCommand =
    WorkerCommand(Vector(resolve(python), shimFile().toString), PyModule.env(modules, env))

  /** the seam the handshake test uses: any shim file */
  private[py] def startWith(python: String, shim: java.nio.file.Path,
                            env: Map[String, String])(using WireFormat, WireCompression, WireDeadline)(using okay.codec.FrameFormat): ForeignWorker =
    startCommand(Vector(resolve(python), shim.toString), python, env)

  /**
   * An engine over ANY process that speaks the okay wire — the handshake
   * line first, then one JSON request and one answer per line
   * (remote-foreign). A compiled Haskell worker built on the `Okay` module
   * this jar ships (`/okay/hs/Okay.hs`) is one: its programs-as-data run
   * through `Py.program` exactly as Python's do, multi-shot included.
   */
  def speaking(command: Seq[String], env: Map[String, String] = Map.empty)
              (using WireFormat, WireCompression, okay.codec.WireAuth, WireDeadline)(using okay.codec.FrameFormat): ForeignWorker =
    startCommand(command.toVector, command.headOption.getOrElse("?"), env)

  /**
   * The engine over ANY link (polyglot-one-wire): the far side speaks the
   * handshake first, then one request and one answer per line. Pipes,
   * a socket, an in-process call — the same `Py.program`, the same
   * callbacks and multi-shot, the same `Durable`.
   */
  def over(link: WireLink, name: String = "the worker")
          (using format: WireFormat, compression: WireCompression, auth: okay.codec.WireAuth,
           deadline: WireDeadline)(using frames: okay.codec.FrameFormat): ForeignWorker =
    new ForeignWorker(WireSession.over(link, name, ShimVersion, "python", "the worker"))

  /** a wire line, read strictly (`okay.codec.WireJson.whole`) */
  private[py] def whole(line: String): Json = WireSession.whole(line)

  /** a worker that is REOPENED by `open` after a death or a deadline, with
   * programs as data recovered by replay (stage 6, `SupervisedWorker`) */
  def supervised(open: => ForeignWorker): SupervisedWorker = SupervisedWorker(() => open)

  /** a worker SERVING the okay wire on TCP (`okay::serve_tcp`, `okay.ServeTCP`):
   * another process, or another machine — plain TCP, see `WireLink.tcp` */
  def connect(host: String, port: Int)
             (using WireFormat, WireCompression, okay.codec.WireAuth, WireDeadline, okay.codec.WireSecurity)(using okay.codec.FrameFormat): ForeignWorker =
    over(WireLink.tcp(host, port, security = summon[okay.codec.WireSecurity],
      helloMillis = summon[WireDeadline].millis.fold(10000)(_.toInt)), s"the worker at $host:$port")

  private def startCommand(cmd: Vector[String], python: String, env: Map[String, String])
                          (using WireFormat, WireCompression, okay.codec.WireAuth, WireDeadline)(using okay.codec.FrameFormat): ForeignWorker =
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

  private def resolve(python: String): String = WireSession.resolve(python)
