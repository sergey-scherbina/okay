package okay.r

import okay.Handler
import okay.codec.{WireAuth, WireCompression, WireDeadline, WireFormat, WireSecurity}
import okay.foreign.{ForeignWorker, SupervisedWorker, WireLink, WireSession}

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
 * Since foreign-one-value R has no engine of its own at all: an R
 * process is a `ForeignWorker` speaking R's shim (the one wire, the one
 * handler, the one value tree read by R's rules), and with a deadline a
 * `SupervisedWorker` over such workers — whose replay of programs as data
 * is the one R used to carry a copy of. So a timeout's respawn, and now a
 * DEATH's too, reopen the same way the first open was made.
 */
final class RSubprocess private (plain: Option[ForeignWorker], supervised: Option[SupervisedWorker],
                                 /** the deadline a call has, if any */
                                 val timeoutMillis: Option[Long]):

  /** R's version, from the shim's hello ("?" until a supervised R opens) */
  val rVersion: String = plain.map(_.pythonVersion).getOrElse("?")

  /** the one foreign handler, over R */
  def handler: Handler[REval] = supervised.fold(plain.get.handler)(_.handler)

  /** what the handshake settled on: "json/none" (JSON lines),
   * "json/zlib", "cbor/zlib", "cbor/none", "+arrow" where frames cross so */
  def wire: String = supervised.fold(plain.get.wire)(_.wire)

  /** frames that crossed as Arrow: (sent, answered) */
  def arrowFrames: (Long, Long) = supervised.fold(plain.get.arrowFrames)(_.arrowFrames)

  /**
   * Presence and version of named packages, mismatches as data naming
   * the package — an analyst's environment drifts, and this turns
   * "wrong forecast, silently" into "loud refusal naming forecast".
   */
  def verify(packages: Map[String, String]): Vector[String] =
    supervised.fold(plain.get.verify(packages))(_.verify(packages))

  def close(): Unit = supervised.fold(plain.get.close())(_.close())

object RSubprocess:

  /** 9: foreign-one-value — the shared value tags, frames announced columnar;
   * 10: foreign-one-program — `start`/`resume` fold into `program`/`continue`;
   * 11: foreign-one-held — `hold` folds into `call` with `held`;
   * 12: foreign-one-protocol — `frame` folds into `call` with `table` */
  val ShimVersion = 12

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
             * takes the next call on a fresh process (r-finish), replaying
             * a program as data it was in the middle of */
            timeoutMillis: Option[Long] = None,
            /** packages this session REQUIRES, name -> version prefix:
             * checked here, at construction, and a drift refuses with
             * the engine never handed out (r-measure-harden) */
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
   * instead of the given-based format/compression/frames above.
   * `wire.deadline` goes unused: a timeout here is `timeoutMillis`. */
  def startWithWire(wire: okay.codec.WireChoice, rscript: String = "Rscript",
                    env: Map[String, String] = Map.empty, timeoutMillis: Option[Long] = None,
                    require: Map[String, String] = Map.empty, modules: Seq[RModule] = Nil): RSubprocess =
    start(rscript, env, timeoutMillis, require, modules)(using wire.format, wire.compression)(using wire.frames)

  /**
   * R SERVED ON THE NETWORK: a worker behind `okay.foreign.ForeignGateway`
   * (`ForeignGateway.start(RSubprocess.command(...))`), with the gateway's
   * TLS and HMAC — `WireSecurity` and `WireAuth` given here exactly as for
   * any other language. A timeout RECONNECTS: the gateway starts a fresh R
   * per connection.
   */
  def connect(host: String, port: Int, timeoutMillis: Option[Long] = None, require: Map[String, String] = Map.empty)
             (using WireFormat, WireCompression, WireAuth, WireSecurity)(using okay.codec.FrameFormat): RSubprocess =
    val sec = summon[WireSecurity]
    def open(): ForeignWorker =
      given WireDeadline = WireDeadline(timeoutMillis)
      speaking(WireLink.tcp(host, port, security = sec, helloMillis = timeoutMillis.fold(10000)(_.toInt)),
        s"the R worker at $host:$port")
    required(engine(() => open(), timeoutMillis), require)

  /** R over any link that speaks the wire, unsupervised */
  def over(link: WireLink, name: String = "the R worker", timeoutMillis: Option[Long] = None)
          (using WireFormat, WireCompression, WireAuth)(using okay.codec.FrameFormat): RSubprocess =
    given WireDeadline = WireDeadline(timeoutMillis)
    new RSubprocess(Some(speaking(link, name)), None, timeoutMillis)

  /** the R worker as a COMMAND, for a process okay does not start itself:
   * the gateway runs one per connection */
  def command(rscript: String = "Rscript", modules: Seq[RModule] = Nil,
              env: Map[String, String] = Map.empty): okay.foreign.WorkerCommand =
    okay.foreign.WorkerCommand(Vector(WireSession.resolve(rscript), "--vanilla", shimFile().toString), RModule.env(modules, env))

  /**
   * R as one more `ForeignWorker` (foreign-one-value): the process
   * `command` starts, spoken to over its pipes, unsupervised — what the
   * conformance suites every wire language answers to take.
   */
  def worker(rscript: String = "Rscript", modules: Seq[RModule] = Nil, env: Map[String, String] = Map.empty)
            (using WireFormat, WireCompression)(using okay.codec.FrameFormat): ForeignWorker =
    val c = command(rscript, modules, env)
    val pb = ProcessBuilder(c.command*)
    pb.environment().clear()
    c.env.foreach((k, v) => pb.environment().put(k, v))
    val proc = pb.start()
    given WireAuth = WireAuth.Off
    given WireDeadline = WireDeadline(None)
    speaking(WireLink.pipes(proc), "the R shim")

  /** the one engine over R's far side: R's shim version, R's name for its
   * version in the hello, R's rules for frames as Arrow and for values */
  def speaking(link: WireLink, name: String)
                      (using WireFormat, WireCompression, WireAuth, WireDeadline)(using okay.codec.FrameFormat): ForeignWorker =
    ForeignWorker.speakingAs(link, name, ShimVersion, "r", "the R process", RArrowFrames, R.shape)

  /** without a deadline the worker itself, whose death THROWS (the
   * supervisor decides — a pool, a caller); with one, supervised: a
   * timeout or a death reopens by `open`, and a program in flight replays */
  private def engine(open: () => ForeignWorker, timeoutMillis: Option[Long]): RSubprocess =
    if timeoutMillis.isEmpty then new RSubprocess(Some(open()), None, None)
    else
      val first = open()
      var fresh = true
      new RSubprocess(Some(first), Some(ForeignWorker.supervised(
        if fresh then { fresh = false; first } else open())), timeoutMillis)

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
                          (using WireFormat, WireCompression)(using okay.codec.FrameFormat): RSubprocess =
    def open(): ForeignWorker =
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
      speaking(WireLink.pipes(proc), "the R shim")
    engine(() => open(), timeoutMillis)
