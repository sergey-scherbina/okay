package okay.r

import okay.Handler
import okay.codec.{Json, WireAuth, WireCompression, WireDeadline, WireFormat, WireSecurity}
import okay.py.{WireLink, WireSession}
import scala.annotation.tailrec

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
 *
 * One edge, stated because a reader will meet it rather than read it
 * (r-measure-harden): if the RESPAWN after a timeout fails — R gone
 * from the environment between two calls, a container stopped — that
 * failure THROWS rather than answering data. It is the dead-process
 * story arriving one step later, and the honest answer is the same
 * one: an engine whose interpreter no longer exists is not something
 * a program can handle as a value.
 *
 * A TIMEOUT is the third outcome (r-finish): R is a language where a
 * plausible call runs for ever (an unconverged optimiser, a regex on
 * a big frame), and the shim is line-oriented, so a host that just
 * blocks on `readLine` waits for ever with it. With `timeoutMillis`
 * set, a call that does not answer in time has its PROCESS killed —
 * the only way to stop R mid-call — a fresh one is started in its
 * place, and the call answers `Left(Condition("timeout", …))`. The
 * respawn is invisible to the program, and that is a property of the
 * no-source design rather than luck: the API has no way to assign
 * anything in the R session, so a fresh process has nothing to have
 * lost. Data,
 * not an exception, and the engine is usable for the next call: the
 * dead-process THROW stays what it is, an engine nobody can revive.
 *
 * The wire is the one every language shares (`okay.py.WireSession`,
 * foreign-one-r): the handshake, the negotiation of stage 5, the deadline,
 * the Arrow road and a death are the session's, and this class is the
 * handler that speaks `RValue` over it — so R reaches a worker over TCP,
 * behind the gateway, with `WireAuth` and `WireSecurity`, exactly as the
 * others do. A timeout's respawn opens a fresh session the same way the
 * first was opened.
 */
final class RSubprocess private (private var session: WireSession,
                                 /** how the engine gets a FRESH session after a
                                  * timeout kills this one — the same opening the
                                  * first one had; `None` where it cannot */
                                 private val respawn: Option[() => WireSession],
                                 val timeoutMillis: Option[Long]):

  /** R's version, from the shim's hello */
  val rVersion: String = session.version

  /** what the handshake settled on: "json/none" (JSON lines),
   * "json/zlib", "cbor/zlib", "cbor/none" */
  def wire: String = session.wire

  /** frames that crossed as Arrow: (sent, answered) — the JSON/CBOR road
   * gives the same values, so this is how a caller (or a test) sees which
   * road a frame took */
  def arrowFrames: (Long, Long) = session.arrowFrames

  /**
   * One step on the session, the timeout as DATA: the session closed its
   * wire at the deadline (the ONLY way to stop R mid-call is to kill the
   * process), and the engine takes the next call on a fresh one.
   */
  private def onSession[T](f: WireSession => T): Either[Condition, T] =
    try Right(f(session))
    catch case t: WireSession.TimedOut =>
      respawn.foreach { fresh =>
        session = fresh()
        generation += 1
      }
      Left(Condition("timeout",
        s"the R call did not answer within ${t.millis}ms — the process was killed" +
          (if respawn.isDefined then " and a fresh one took its place" else "")))

  private def exchange(req: Json): Either[Condition, Json] = onSession(_.exchange(req))

  /** one message out, the next in — a `resume` opens nothing, so it
   * carries no id (foreign-callbacks) */
  private def send(body: Json): Either[Condition, Json] = onSession(_.send(body))

  private def answer[A](e: Either[Condition, Json])(ok: Json => Either[Condition, A]): Either[Condition, A] =
    e.flatMap(j => WireSession.answer(j)(Condition(_, _))(ok))

  // ---- programs as data survive a respawn (r-supervised-replay) --------

  /** how many times this engine's R was replaced (a timeout's respawn) */
  private var generation = 0L

  /** a continuation the CALLER holds: its run, the path of answers from the
   * program's start, the operation it stands at, and where it lives now */
  private final case class Kont(run: Long, path: Vector[RValue], op: String, args: Vector[RValue],
                                var local: Long, var gen: Long)
  private val runs = scala.collection.mutable.Map.empty[Long, (String, Vector[RValue])]
  private val konts = scala.collection.mutable.Map.empty[Long, Kont]
  private var nextK = 0L

  private def rawProgram(run: Long, fn: String, args: Vector[RValue]): Either[Condition, RNode] =
    answer(exchange(Json.JObj(Vector(
      "op" -> Json.JStr("program"), "run" -> Json.JNum(run.toDouble), "fn" -> Json.JStr(fn),
      "args" -> Json.JArr(args.map(Wire.enc))))))(Wire.decNode)

  private def rawContinue(run: Long, k: Long, a: RValue): Either[Condition, RNode] =
    answer(exchange(Json.JObj(Vector(
      "op" -> Json.JStr("continue"), "run" -> Json.JNum(run.toDouble), "k" -> Json.JNum(k.toDouble),
      "answer" -> Wire.enc(a)))))(Wire.decNode)

  /** a node from R, its `k` renamed to one the caller keeps across a respawn */
  private def node(run: Long, path: Vector[RValue], n: RNode): RNode = n match
    case RNode.Perform(op, args, k) =>
      nextK += 1
      konts(nextK) = Kont(run, path, op, args, k, generation)
      RNode.Perform(op, args, nextK)
    case done => done

  /**
   * On the CURRENT R: the program re-run and `path` replayed, to the
   * continuation standing at `op(args)`. An R program-as-data is a pure
   * function of its answers, so this re-derives what the killed process
   * held; a replay that meets another operation is `ReplayDrift`, never a
   * wrong answer. The same road `SupervisedWorker` takes for ForeignWorker.
   */
  private def replay(c: Kont): Either[Condition, Long] =
    val (fn, fnArgs) = runs(c.run)
    // one loop over the recorded path, not a frame per answer: a durable
    // run replays as many steps as it journaled (stack-safety-py-r)
    def step(n0: Either[Condition, RNode], rest0: Vector[RValue]): Either[Condition, Long] =
      var n = n0
      var rest = rest0
      var result: Option[Either[Condition, Long]] = None
      while result.isEmpty do n match
        case Left(cond) => result = Some(Left(cond))
        case Right(RNode.Perform(o, as, k)) if rest.isEmpty =>
          result = Some(if o == c.op && as == c.args then Right(k)
            else Left(Condition("ReplayDrift",
              s"replaying run ${c.run} met $o$as where the path recorded ${c.op}${c.args}: the R program is not a pure function of its answers")))
        case Right(RNode.Perform(_, _, k)) => n = rawContinue(c.run, k, rest.head); rest = rest.tail
        case Right(RNode.Done(v)) =>
          result = Some(Left(Condition("ReplayDrift", s"replaying run ${c.run} finished ($v) before the recorded path did")))
      result.get
    step(rawProgram(c.run, fn, fnArgs), c.path)

  private def continueRun(k: Long, a: RValue): Either[Condition, RNode] =
    konts.get(k) match
      case None => Left(Condition("LookupError", s"continuation $k is not held (forgotten?)"))
      case Some(c) =>
        @tailrec def attempt(again: Boolean): Either[Condition, RNode] =
          val local =
            if c.gen == generation then Right(c.local)
            else replay(c).map { l => c.local = l; c.gen = generation; l }
          val before = generation
          local.flatMap(l => rawContinue(c.run, l, a)) match
            // the step itself outlived the deadline and R was replaced: redo it once, by replay
            case Left(cond) if again && generation != before && cond.kind == "timeout" => attempt(again = false)
            case other => other.map(n => node(c.run, c.path :+ a, n))
        attempt(again = true)

  /** the comonadic handler — one operation, one exchange */
  def handler: Handler[REval] = new:
    def handle[A](e: REval[A]): A = e match
      case REval.Call(fn, args) =>
        answer(exchange(Json.JObj(Vector(
          "op" -> Json.JStr("call"), "fn" -> Json.JStr(fn),
          "args" -> Json.JArr(args.map(Wire.enc))))))(v => Right(Wire.dec(v)))
      case REval.Frame(fn, frame, args) =>
        val head = Vector("op" -> Json.JStr("frame"), "fn" -> Json.JStr(fn), "args" -> Json.JArr(args.map(Wire.enc)))
        val table = if session.arrow then RArrowFrames.table(frame) else Left("")
        table match
          case Right(t) =>
            onSession(_.exchangeArrow(head, t)) match
              case Left(c) => Left(c)
              case Right((j, got)) => answer(Right(j))(v => got.fold(Wire.decFrame(v))(t => Right(RArrowFrames.frame(t))))
          case Left(why) if session.arrow && session.frames.strict =>
            Left(Condition("NotArrow", s"this host's given FrameFormat is arrow, and $why"))
          case Left(_) =>
            answer(exchange(Json.JObj(head :+ ("in" -> Wire.encFrame(frame)))))(Wire.decFrame)
      case REval.Start(fn, args, cbs) =>
        stepOf(exchange(Json.JObj(Vector(
          "op" -> Json.JStr("start"), "fn" -> Json.JStr(fn),
          "args" -> Json.JArr(args.map(Wire.enc)),
          "callbacks" -> Json.JArr(cbs.map(Json.JStr(_)))))))
      case REval.Resume(k, a) =>
        val answered = a match
          case Right(v) => "ok" -> Wire.enc(v)
          case Left(c) => "condition" -> Json.JObj(Vector(
            "kind" -> Json.JStr(c.kind), "message" -> Json.JStr(c.message)))
        stepOf(send(Json.JObj(Vector(
          "op" -> Json.JStr("resume"), "k" -> Json.JNum(k.toDouble), answered))))
      case REval.Hold(fn, args) =>
        answer(exchange(Json.JObj(Vector(
          "op" -> Json.JStr("hold"), "fn" -> Json.JStr(fn),
          "args" -> Json.JArr(args.map(Wire.enc))))))(v => Wire.asRef(Wire.dec(v)))
      case REval.Program(run, fn, args) =>
        runs(run) = (fn, args)
        rawProgram(run, fn, args).map(n => node(run, Vector.empty, n))
      case REval.Continue(_, k, a) =>
        continueRun(k, a)
      case REval.Forget(run) =>
        runs.remove(run): Unit
        konts.filterInPlace((_, c) => c.run != run)
        val _ = exchange(Json.JObj(Vector("op" -> Json.JStr("forget"), "run" -> Json.JNum(run.toDouble))))
      case REval.Release(r) =>
        // idempotent: a release of a ref the process does not hold is not
        // a program's concern (a timeout's respawn already dropped it)
        val _ = exchange(Json.JObj(Vector("op" -> Json.JStr("release"), "ref" -> Json.JNum(r.id.toDouble))))

  /** a call's next message: an ask, or its answer (a timeout included) */
  private def stepOf(e: Either[Condition, Json]): RStep =
    e.toOption.flatMap(Wire.step).getOrElse(RStep.Done(answer(e)(v => Right(Wire.dec(v)))))

  /**
   * Presence and version of named packages, mismatches as data naming
   * the package — an analyst's environment drifts, and this turns
   * "wrong forecast, silently" into "loud refusal naming forecast".
   */
  def verify(packages: Map[String, String]): Vector[String] =
    onSession(_.verify(packages)).fold(c => Vector(s"verify itself failed: ${c.kind}: ${c.message}"), identity)

  def close(): Unit = session.close()

object RSubprocess:

  val ShimVersion = 8

  /**
   * Start a session: the configured `Rscript` (resolved against PATH
   * when relative — the child's env is empty, so resolution happens
   * HERE), the shim from this jar, a CLEAN environment plus exactly
   * what `env` names.
   */
  def start(rscript: String = "Rscript",
            env: Map[String, String] = Map.empty,
            /** a call that does not answer in this long has its process
             * killed and answers `Condition("timeout", …)`; the engine
             * takes the next call on a fresh process (r-finish) */
            timeoutMillis: Option[Long] = None,
            /** packages this session REQUIRES, name -> version prefix:
             * checked here, at construction, and a drift refuses with
             * the engine never handed out (r-measure-harden). The same
             * verify-at-startup posture the Sql seam takes, for the
             * same reason — an analyst's environment drifts, and the
             * alternative to a loud refusal is a wrong number later */
            require: Map[String, String] = Map.empty,
            /** inline modules to load at start (foreign-inline-modules) */
            modules: Seq[RModule] = Nil)
           /** the wire's format and compression (wire-givens-r): JSON, with
            * zlib where R has it, unless an import says otherwise */
           (using WireFormat, WireCompression)
           /** whether frames cross as Arrow (r-arrow): where spoken, else
            * JSON/CBOR, unless an import says otherwise — the R shim
            * announces arrow when the `arrow` package is installed */
           (using okay.codec.FrameFormat): RSubprocess =
    required(startWith(rscript, shimFile(), RModule.env(modules, env), timeoutMillis), require)

  /** `start`, with the wire picked by an explicit `okay.codec.WireChoice`
   * instead of the given-based format/compression/frames above — for a
   * caller that decides the wire at RUNTIME, from a string (a flag, a
   * config file): `WireChoice.named(format = cfg.format, frames =
   * cfg.frames)`. `wire.deadline` goes unused: a timeout here is
   * `timeoutMillis`, its own parameter, not part of the wire's codec. */
  def startWithWire(wire: okay.codec.WireChoice, rscript: String = "Rscript",
                    env: Map[String, String] = Map.empty, timeoutMillis: Option[Long] = None,
                    require: Map[String, String] = Map.empty, modules: Seq[RModule] = Nil): RSubprocess =
    start(rscript, env, timeoutMillis, require, modules)(using wire.format, wire.compression)(using wire.frames)

  /**
   * R SERVED ON THE NETWORK: a worker behind `okay.py.ForeignGateway`
   * (`ForeignGateway.start(RSubprocess.command(...))`), on this machine or
   * another, with the gateway's TLS and HMAC — `WireSecurity` and
   * `WireAuth` given here exactly as for any other language. A timeout
   * RECONNECTS: the gateway starts a fresh R per connection.
   */
  def connect(host: String, port: Int, timeoutMillis: Option[Long] = None, require: Map[String, String] = Map.empty)
             (using WireFormat, WireCompression, WireAuth, WireSecurity)(using okay.codec.FrameFormat): RSubprocess =
    val sec = summon[WireSecurity]
    def open(): WireSession =
      given WireDeadline = WireDeadline(timeoutMillis)
      WireSession.over(WireLink.tcp(host, port, security = sec, helloMillis = timeoutMillis.fold(10000)(_.toInt)),
        s"the R worker at $host:$port", ShimVersion, "r", "the R process")
    required(new RSubprocess(open(), Some(() => open()), timeoutMillis), require)

  /** R over any link that speaks the wire (the session refuses by name
   * whatever it cannot), with no respawn: a timeout leaves it dead */
  def over(link: WireLink, name: String = "the R worker", timeoutMillis: Option[Long] = None)
          (using WireFormat, WireCompression, WireAuth)(using okay.codec.FrameFormat): RSubprocess =
    given WireDeadline = WireDeadline(timeoutMillis)
    new RSubprocess(WireSession.over(link, name, ShimVersion, "r", "the R process"), None, timeoutMillis)

  /** the R worker as a COMMAND, for a process okay does not start itself:
   * the gateway runs one per connection */
  def command(rscript: String = "Rscript", modules: Seq[RModule] = Nil,
              env: Map[String, String] = Map.empty): okay.py.WorkerCommand =
    okay.py.WorkerCommand(Vector(WireSession.resolve(rscript), "--vanilla", shimFile().toString), RModule.env(modules, env))

  /** the shim from this jar, as a file a process can run */
  private def shimFile(): java.nio.file.Path =
    WireSession.shipped(classOf[RSubprocess], "/okay/r/shim.R", "okay-r-shim", ".R")

  /** the engine handed out only if the environment meets `require` */
  private def required(engine: RSubprocess, require: Map[String, String]): RSubprocess =
    if require.isEmpty then engine
    else
      val drift = engine.verify(require)
      if drift.isEmpty then engine
      else
        engine.close()
        throw IllegalStateException(
          s"the R environment does not meet what this session requires:\n  " +
            drift.mkString("\n  "))

  /** the seam the handshake test uses: any shim file */
  private[r] def startWith(rscript: String, shim: java.nio.file.Path,
                           env: Map[String, String],
                           timeoutMillis: Option[Long] = None)
                          (using WireFormat, WireCompression)(using frames: okay.codec.FrameFormat): RSubprocess =
    // the respawn a timeout needs is this very opening — a fresh process
    // of the same shape, which negotiates the same wire again
    def open(): WireSession = session(rscript, shim, env, timeoutMillis)
    new RSubprocess(open(), Some(() => open()), timeoutMillis)

  private def session(rscript: String, shim: java.nio.file.Path, env: Map[String, String], timeoutMillis: Option[Long])
                     (using WireFormat, WireCompression)(using okay.codec.FrameFormat): WireSession =
    // --vanilla: no site file, no profile, no saved workspace — the
    // clean-environment rule extended to R's OWN startup, which reads
    // four files by default and would otherwise import an analyst's
    // options into every call
    val pb = ProcessBuilder(WireSession.resolve(rscript), "--vanilla", shim.toString)
    pb.environment().clear()
    env.foreach((k, v) => pb.environment().put(k, v))
    pb.redirectErrorStream(false)
    val proc =
      try pb.start()
      catch case e: java.io.IOException =>
        throw IllegalStateException(
          s"'$rscript' did not start: ${e.getMessage} — the wrong-environment refusal, at its loudest")
    // a pipe to a process okay started: nobody to authenticate
    given WireAuth = WireAuth.Off
    given WireDeadline = WireDeadline(timeoutMillis)
    // the handshake: the shim speaks first, and drift refuses loudly
    WireSession.over(WireLink.pipes(proc), "the R shim", ShimVersion, "r", "the R process")
