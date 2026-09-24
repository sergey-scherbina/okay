package okay.r

import java.io.{BufferedInputStream, BufferedOutputStream}
import okay.Handler
import okay.codec.{Json, WireCompression, WireFormat, WireFrames, WireNegotiation}

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
 */
final class RSubprocess private (private var proc: Process,
                                 private var out: BufferedOutputStream,
                                 private var in: BufferedInputStream,
                                 /** the configured format and compression
                                  * (wire-givens-r); None: JSON lines */
                                 private var codec: Option[(WireFormat, WireCompression)],
                                 val rVersion: String,
                                 /** how the engine gets a FRESH process after a
                                  * timeout kills this one; `None` for a handle
                                  * that cannot respawn (the handshake test's) */
                                 private val respawn: Option[() => RSubprocess.Parts],
                                 val timeoutMillis: Option[Long]):

  /** what the handshake settled on: "json/none" (JSON lines),
   * "json/zlib", "cbor/zlib", "cbor/none" */
  def wire: String = codec.fold("json/none")((f, c) => s"${f.name}/${c.name}")

  private var nextId = 0
  /** one daemon thread per engine, and only where a timeout asks for
   * it: the blocking `readLine` has to happen off the caller's thread
   * for the caller to be able to give up on it */
  private lazy val reader: java.util.concurrent.ExecutorService =
    java.util.concurrent.Executors.newSingleThreadExecutor { r =>
      val t = Thread(r, "okay-r-reader"); t.setDaemon(true); t
    }

  private def exchange(req: Json): Either[Condition, Json] =
    nextId += 1
    val id = nextId
    val body = req match
      case Json.JObj(fs) => Json.JObj(("id" -> Json.JNum(id.toDouble)) +: fs)
      case other => other
    send(body)

  /** one message out, the next in — a `resume` opens nothing, so it
   * carries no id (foreign-callbacks) */
  private def send(body: Json): Either[Condition, Json] =
    codec match
      case None => WireFrames.writeLine(out, Json.print(body))
      case Some((f, c)) => WireFrames.writeFrame(out, c.compress(f.encode(body)))
    readMessage() match
      case Left(c) => Left(c)
      case Right(None) =>
        throw IllegalStateException(
          "the R process is DEAD (eof on the wire) — a supervisor retry gets a fresh one")
      case Right(Some(answer)) => Right(answer)

  /** the next message off the wire, as this engine's codec reads it; None
   * at the stream's end */
  private def read(): Option[Json] =
    val (i, c) = (in, codec)
    c match
      case None => WireFrames.readLine(i).map(Json.parse)
      case Some((f, z)) => WireFrames.readFrame(i).map(bytes => f.decode(z.decompress(bytes)))

  /** the answer, or the timeout as data. Without a deadline this is
   * `read()` and nothing else happens. */
  private def readMessage(): Either[Condition, Option[Json]] = timeoutMillis match
    case None => Right(read())
    case Some(ms) =>
      val task = reader.submit(() => read())
      try Right(task.get(ms, java.util.concurrent.TimeUnit.MILLISECONDS))
      catch
        case _: java.util.concurrent.TimeoutException =>
          task.cancel(true): Unit
          // the ONLY way to stop R mid-call, and it takes the wire
          // with it — so the engine gets a new process or becomes one
          // nobody can revive
          proc.destroyForcibly().waitFor(): Unit
          respawn match
            case Some(fresh) =>
              val parts = fresh()
              proc = parts.proc; out = parts.out; in = parts.in; codec = parts.codec
            case None => ()
          Left(Condition("timeout",
            s"the R call did not answer within ${ms}ms — the process was killed" +
              (if respawn.isDefined then " and a fresh one took its place" else "")))

  private def answer[A](e: Either[Condition, Json])(ok: Json => Either[Condition, A]): Either[Condition, A] =
    e.flatMap { j => j match
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
    }

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
        answer(exchange(Json.JObj(Vector(
          "op" -> Json.JStr("program"), "run" -> Json.JNum(run.toDouble), "fn" -> Json.JStr(fn),
          "args" -> Json.JArr(args.map(Wire.enc))))))(Wire.decNode)
      case REval.Continue(run, k, a) =>
        answer(exchange(Json.JObj(Vector(
          "op" -> Json.JStr("continue"), "run" -> Json.JNum(run.toDouble), "k" -> Json.JNum(k.toDouble),
          "answer" -> Wire.enc(a)))))(Wire.decNode)
      case REval.Forget(run) =>
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

  /** the fresh process's three parts, for a handle that is replacing
   * its own (r-finish): the new handle is abandoned after this, so
   * nothing is closed twice */
  private[r] def take(): RSubprocess.Parts =
    RSubprocess.Parts(proc, out, in, codec)

  def close(): Unit =
    try { out.close(); in.close() } catch case _: Exception => ()
    proc.destroy()
    if timeoutMillis.isDefined then reader.shutdownNow(): Unit

object RSubprocess:

  val ShimVersion = 7

  /** a live process and its wire: what a timeout's respawn swaps in */
  private[r] final case class Parts(proc: Process, out: BufferedOutputStream, in: BufferedInputStream,
                                    codec: Option[(WireFormat, WireCompression)])

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
           (using WireFormat, WireCompression): RSubprocess =
    val shim = java.nio.file.Files.createTempFile("okay-r-shim", ".R")
    val res = getClass.getResourceAsStream("/okay/r/shim.R")
    if res == null then throw IllegalStateException("the shim resource is missing from the jar")
    try java.nio.file.Files.copy(res, shim, java.nio.file.StandardCopyOption.REPLACE_EXISTING)
    finally res.close()
    shim.toFile.deleteOnExit()
    val engine = startWith(rscript, shim, RModule.env(modules, env), timeoutMillis)
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
                          (using WireFormat, WireCompression): RSubprocess =
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
    val out = BufferedOutputStream(proc.getOutputStream)
    val in = BufferedInputStream(proc.getInputStream)

    // the handshake: the shim speaks first, and drift refuses loudly
    val hello = WireFrames.readLine(in).getOrElse {
      throw IllegalStateException(s"'$rscript' started but the shim answered nothing (stderr may know)")
    }
    val helloJson = Json.parse(hello)
    val fields = helloJson match
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
    // stage 5: the givens against what the shim announced, confirmed by a
    // configure line; a choice R does not speak is refused by name
    def refuse(why: String): Nothing =
      proc.destroy()
      throw IllegalStateException(why)
    val codec = WireNegotiation.choose(helloJson, inProcess = false, "the R shim") match
      case Left(why) => refuse(why)
      case Right(None) => None
      case Right(Some((f, c))) =>
        WireFrames.writeLine(out, WireNegotiation.configure(f, c))
        WireNegotiation.confirmed("the R shim", f, c, WireFrames.readLine(in).map(Json.parse))
          .fold(refuse, _ => Some((f, c)))
    // the respawn a timeout needs is this very function, minus the
    // handshake's refusals — a fresh process of the same shape, which
    // negotiates the same wire again
    new RSubprocess(proc, out, in, codec, one("r").getOrElse("?"),
      Some(() => {
        val again = startWith(rscript, shim, env, None)
        (again.take())
      }),
      timeoutMillis)

  private def resolve(rscript: String): String =
    if rscript.contains("/") then rscript
    else
      sys.env.getOrElse("PATH", "").split(":").iterator
        .map(d => java.nio.file.Paths.get(d, rscript))
        .find(p => java.nio.file.Files.isExecutable(p))
        .map(_.toString)
        .getOrElse(rscript)   // let start() produce the loud refusal
