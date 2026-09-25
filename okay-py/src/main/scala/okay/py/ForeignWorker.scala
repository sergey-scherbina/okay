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
final class ForeignWorker private (link: WireLink, val pythonVersion: String,
                                   /** the configured format and compression; None: JSON lines */
                                   codec: Option[(WireFormat, WireCompression)],
                                   /** how long an answer may take (stage 6); None: for ever */
                                   deadline: Option[Long],
                                   /** whether frames cross as Arrow (py-arrow), and how strictly */
                                   arrow: Boolean, frames: okay.codec.FrameFormat):

  private var nextId = 0

  /** false once the wire is gone: an end of stream, or a deadline that
   * closed it. A supervisor reads this to know it must reopen. */
  @volatile private var live = true
  def alive: Boolean = live

  /** the reads a deadline can give up on happen off the caller's thread,
   * one daemon per engine, made only when a deadline asks for it */
  private lazy val reader: java.util.concurrent.ExecutorService =
    java.util.concurrent.Executors.newSingleThreadExecutor { r =>
      val t = Thread(r, "okay-wire-reader"); t.setDaemon(true); t
    }

  /** one exchange on the link, within the deadline if there is one */
  private def io[T](f: => T): T = deadline match
    case None => f
    case Some(ms) =>
      val task = reader.submit(() => f)
      try task.get(ms, java.util.concurrent.TimeUnit.MILLISECONDS)
      catch
        case _: java.util.concurrent.TimeoutException =>
          task.cancel(true): Unit
          // the only way to abandon a blocked read: take the wire with it
          live = false
          link.close()
          throw ForeignWorker.TimedOut(ms)
        case e: java.util.concurrent.ExecutionException => throw e.getCause

  /** what the handshake settled on: "json/none" (the plain JSON lines),
   * "json/deflate", "cbor/none", "cbor/deflate" */
  def wire: String = codec.fold("json/none")((f, c) => s"${f.name}/${c.name}") + (if arrow then "+arrow" else "")

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
    codec match
      case None => onTheWire(link.roundTrip(Json.print(body)).map(ForeignWorker.whole))
      case Some((format, compression)) =>
        format.decode(compression.decompress(onTheWire(link.exchange(compression.compress(format.encode(body))))))

  /** one frame as ONE Arrow stream, its header in the schema's metadata
   * (py-arrow); the answer is Arrow again, or an ordinary message (a
   * condition, or a frame Arrow could not carry) */
  @volatile private var arrowOut = 0L
  @volatile private var arrowIn = 0L
  /** frames that crossed as Arrow: (sent, answered) — the JSON road
   * gives the same values, so this is how a caller (or a test) sees
   * which road a frame took */
  def arrowFrames: (Long, Long) = (arrowOut, arrowIn)

  private def sendArrow(body: Json, table: okay.arrow.Table): (Json, Option[PyFrame]) =
    val (format, compression) = codec.getOrElse(throw IllegalStateException("an Arrow frame on an unframed wire"))
    val header = table.copy(metadata = Vector("okay" -> Json.print(body)))
    val bytes = compression.decompress(onTheWire(link.exchange(compression.compress(okay.arrow.OkayArrow.write(header)))))
    arrowOut += 1
    if okay.arrow.ArrowCodec.isStream(bytes) then
      arrowIn += 1
      val t = okay.arrow.OkayArrow.read(bytes)
      val head = t.metadata.collectFirst { case ("okay", h) => ForeignWorker.whole(h) }
        .getOrElse(throw IllegalStateException("an Arrow answer without its okay header"))
      (head, Some(ArrowFrames.frame(t)))
    else (format.decode(bytes), None)

  /** one exchange on the link: a death becomes the DEAD the supervisor reads */
  private def onTheWire[T](f: => Option[T]): T =
    if !live then throw IllegalStateException("the worker is DEAD (its wire was closed) — a supervisor retry gets a fresh one")
    val answer =
      try io(f)
      catch case e: java.io.IOException =>
        // a far side killed from outside (an OOM kill, a crash) does not
        // always end the stream cleanly: the JDK closes a dead child's
        // pipes, a peer resets its socket, and the WRITE throws "Stream
        // closed" or "Broken pipe" (supervised-crash-every-language)
        live = false
        throw IllegalStateException(s"the worker is DEAD (its wire broke: ${e.getMessage}) — a supervisor retry gets a fresh one")
    answer.getOrElse {
      live = false
      throw IllegalStateException("the worker is DEAD (eof on the wire) — a supervisor retry gets a fresh one")
    }

  /** a timeout is DATA for an operation that answers an Either: the call
   * failed, the program can see it and decide (stage 6) */
  private def timed[A](f: => Either[Condition, A]): Either[Condition, A] =
    try f catch case t: ForeignWorker.TimedOut => Left(Condition("timeout", t.getMessage))

  private def timedStep(f: => PyStep): PyStep =
    try f catch case t: ForeignWorker.TimedOut => PyStep.Done(Left(Condition("timeout", t.getMessage)))

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
      case ForeignEval.Call(fn, args) => timed:
        answer(exchange(Json.JObj(Vector(
          "op" -> Json.JStr("call"), "fn" -> Json.JStr(fn),
          "args" -> Json.JArr(args.map(Wire.enc))))))(v => Right(Wire.dec(v)))
      case ForeignEval.Frame(fn, frame, args) => timed:
        val head = Vector("op" -> Json.JStr("frame"), "fn" -> Json.JStr(fn), "args" -> Json.JArr(args.map(Wire.enc)))
        val table = if arrow then ArrowFrames.table(frame) else Left("")
        table match
          case Right(t) =>
            nextId += 1
            val (j, got) = sendArrow(Json.JObj(("id" -> Json.JNum(nextId.toDouble)) +: head), t)
            answer(j)(v => got.fold(Wire.decFrame(v))(Right(_)))
          case Left(why) if arrow && frames.strict =>
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
    live = false
    link.close()
    if deadline.isDefined then reader.shutdownNow(): Unit

object ForeignWorker:

  /** an answer that did not come within the deadline; the message says DEAD
   * so a supervisor that retires dead workers (`PyWorkers`) retires it */
  final class TimedOut(val millis: Long) extends IllegalStateException(
    s"the worker is DEAD: no answer within ${millis}ms, so its wire was closed — a supervisor gets a fresh one")

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

  /** `start` once the modules are already in the environment */
  private[py] def startIn(python: String, env: Map[String, String])(using WireFormat, WireCompression, WireDeadline)(using okay.codec.FrameFormat): ForeignWorker =
    startWith(python, shimFile(), env)

  /** the shim from this jar, as a file a process can run */
  private def shimFile(): java.nio.file.Path =
    val shim = java.nio.file.Files.createTempFile("okay-py-shim", ".py")
    val res = getClass.getResourceAsStream("/okay/py/shim.py")
    if res == null then throw IllegalStateException("the shim resource is missing from the jar")
    try java.nio.file.Files.copy(res, shim, java.nio.file.StandardCopyOption.REPLACE_EXISTING)
    finally res.close()
    shim.toFile.deleteOnExit()
    shim

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
    if link.inProcess && deadline.millis.isDefined then
      link.close()
      throw IllegalStateException(
        s"$name is in this process: a call into it runs on the caller's thread and cannot be abandoned, " +
          "so a given WireDeadline cannot be kept here — refused rather than promised")
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
    // stage 5b: who may speak, before what the wire looks like
    okay.codec.WireNegotiation.authenticate(whole(hello), name, line => link.roundTrip(line).map(whole))
      .left.foreach { why =>
        link.close()
        throw IllegalStateException(why)
      }
    val (codec, arrow) = configure(link, name, whole(hello))
    new ForeignWorker(link, pyV, codec, deadline.millis, arrow, frames)

  /** stage 5's handshake (`okay.codec.WireNegotiation`): what the givens
   * ask for, checked against what the far side announced, and confirmed */
  private def configure(link: WireLink, name: String, hello: Json)
                       (using WireFormat, WireCompression, okay.codec.FrameFormat): (Option[(WireFormat, WireCompression)], Boolean) =
    def refuse(why: String): Nothing =
      link.close()
      throw IllegalStateException(why)
    val arrow = okay.codec.WireNegotiation.chooseFrames(hello, name).fold(refuse, identity)
    // Arrow needs frames on the wire, so it configures even json/none
    val chosen = okay.codec.WireNegotiation.choose(hello, link.network, name) match
      case Left(why) => refuse(why)
      case Right(None) if !arrow => None
      case Right(None) => Some((WireFormat.json, WireCompression.Off.off))
      case Right(some) => some
    chosen match
      case None => (None, false)
      case Some((f, c)) =>
        okay.codec.WireNegotiation.confirmed(name, f, c,
          link.roundTrip(okay.codec.WireNegotiation.configure(f, c, arrow)).map(whole))
          .fold(refuse, _ => (Some((f, c)), arrow))

  /** a wire line, read strictly (`okay.codec.WireJson.whole`) */
  private[py] def whole(line: String): Json = okay.codec.WireJson.whole(line)

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

  private def resolve(python: String): String =
    if python.contains("/") then python
    else
      sys.env.getOrElse("PATH", "").split(":").iterator
        .map(d => java.nio.file.Paths.get(d, python))
        .find(p => java.nio.file.Files.isExecutable(p))
        .map(_.toString)
        .getOrElse(python)   // let start() produce the loud refusal
