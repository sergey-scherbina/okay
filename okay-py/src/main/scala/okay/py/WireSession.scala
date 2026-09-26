package okay.py

import okay.codec.{Json, WireAuth, WireNegotiation}

/**
 * THE ENGINE UNDER EVERY WIRE LANGUAGE (specs/foreign-one.md, stage 1):
 * one far side on one `WireLink`, whatever the language — the handshake
 * (the shim's version, a `fatal` it names, the auth of stage 5b, the
 * format, compression and frames of stage 5a), then one message out and
 * one in, a deadline that takes the wire with it, an Arrow road for a
 * table, and a broken or ended wire read as a DEAD worker.
 *
 * It knows nothing of a language's VALUES: a request and an answer are
 * `Json` trees, and a language's handler (`ForeignWorker` for Python,
 * TypeScript, Haskell, Go and Rust; `okay.r.RSubprocess` for R) encodes
 * its own values into them. Until foreign-one-r those were two engines,
 * each with its own copy of everything below.
 */
final class WireSession private (link: WireLink,
                                 /** the far side's own version, from its hello */
                                 val version: String,
                                 /** the configured format and compression; None: JSON lines */
                                 codec: Option[(WireFormat, WireCompression)],
                                 /** how long an answer may take (stage 6); None: for ever */
                                 deadline: Option[Long],
                                 /** whether tables cross as Arrow (py-arrow, r-arrow) */
                                 val arrow: Boolean,
                                 /** how strictly: a strict given refuses a frame Arrow cannot carry */
                                 val frames: okay.codec.FrameFormat,
                                 /** who dies, in a death's message: "the worker", "the R process" */
                                 who: String,
                                 /** the far side reads a frame's COLUMNAR shape
                                  * (it announced `"frames": ["columnar"]`) */
                                 val columnar: Boolean):

  private var nextId = 0

  /** false once the wire is gone: an end of stream, a break, or a deadline
   * that closed it. A supervisor reads this to know it must reopen. */
  @volatile private var live = true
  def alive: Boolean = live

  /** the reads a deadline can give up on happen off the caller's thread,
   * one daemon per session, made only when a deadline asks for it */
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
          throw WireSession.TimedOut(ms)
        case e: java.util.concurrent.ExecutionException => throw e.getCause

  /** what the handshake settled on: "json/none" (the plain JSON lines),
   * "json/deflate", "cbor/zlib", …, and "+arrow" where tables cross as an
   * Arrow stream, "+cdata" where they cross in place (foreign-arrow-ffm) */
  def wire: String = codec.fold("json/none")((f, c) => s"${f.name}/${c.name}") +
    (if link.tables.isDefined then "+cdata" else if arrow then "+arrow" else "")

  /** a request that OPENS something: numbered, sent, its answer read */
  def exchange(req: Json): Json =
    nextId += 1
    val body = req match
      case Json.JObj(fs) => Json.JObj(("id" -> Json.JNum(nextId.toDouble)) +: fs)
      case other => other
    send(body)

  /** one message out, the next one in — `exchange` without an id, which
   * is how a `resume` goes: it answers an ask, it opens nothing */
  def send(body: Json): Json =
    codec match
      case None => onTheWire(link.roundTrip(Json.print(body)).map(WireSession.whole))
      case Some((format, compression)) =>
        format.decode(compression.decompress(onTheWire(link.exchange(compression.compress(format.encode(body))))))

  @volatile private var arrowOut = 0L
  @volatile private var arrowIn = 0L
  /** tables that crossed as Arrow: (sent, answered) — the JSON road gives
   * the same values, so this is how a caller (or a test) sees which road a
   * table took */
  def arrowFrames: (Long, Long) = (arrowOut, arrowIn)

  /**
   * A request carrying ONE table, as ONE Arrow stream with the request in
   * the schema's metadata (py-arrow): numbered like `exchange`, answered
   * by the table the far side sent back as Arrow, or by an ordinary
   * message (a condition, or a table Arrow could not carry). Only where
   * the handshake settled on `arrow`.
   */
  def exchangeArrow(head: Vector[(String, Json)], table: okay.arrow.Table): (Json, Option[okay.arrow.Table]) =
    link.tables match
      case Some(native) => exchangeNative(native, head, table)
      case None => exchangeStream(head, table)

  /** the table beside the message, as itself: a link that reads it in place
   * (foreign-arrow-ffm) — the message in the wire's format, no Arrow stream */
  private def exchangeNative(native: WireLink.Tables, head: Vector[(String, Json)],
                             table: okay.arrow.Table): (Json, Option[okay.arrow.Table]) =
    nextId += 1
    val body = Json.JObj(("id" -> Json.JNum(nextId.toDouble)) +: head)
    val (bytes, got) = onTheWire(Some(codec match
      case None => native.exchange(Json.print(body).getBytes(java.nio.charset.StandardCharsets.UTF_8), table)
      case Some((format, compression)) =>
        val (b, t) = native.exchange(compression.compress(format.encode(body)), table)
        (compression.decompress(b), t)))
    arrowOut += 1
    got.foreach(_ => arrowIn += 1)
    val answer = codec match
      case None => WireSession.whole(String(bytes, java.nio.charset.StandardCharsets.UTF_8))
      case Some((format, _)) => format.decode(bytes)
    (answer, got)

  private def exchangeStream(head: Vector[(String, Json)], table: okay.arrow.Table): (Json, Option[okay.arrow.Table]) =
    val (format, compression) = codec.getOrElse(throw IllegalStateException("an Arrow table on an unframed wire"))
    nextId += 1
    val body = Json.JObj(("id" -> Json.JNum(nextId.toDouble)) +: head)
    val header = table.copy(metadata = Vector("okay" -> Json.print(body)))
    val bytes = compression.decompress(onTheWire(link.exchange(compression.compress(okay.arrow.OkayArrow.write(header)))))
    arrowOut += 1
    if okay.arrow.ArrowCodec.isStream(bytes) then
      arrowIn += 1
      val t = okay.arrow.OkayArrow.read(bytes)
      val answer = t.metadata.collectFirst { case ("okay", h) => WireSession.whole(h) }
        .getOrElse(throw IllegalStateException("an Arrow answer without its okay header"))
      (answer, Some(t))
    else (format.decode(bytes), None)

  /** one exchange on the link: a death becomes the DEAD a supervisor reads */
  private def onTheWire[T](f: => Option[T]): T =
    if !live then throw IllegalStateException(s"$who is DEAD (its wire was closed) — a supervisor retry gets a fresh one")
    val answer =
      try io(f)
      catch case e: java.io.IOException =>
        // a far side killed from outside (an OOM kill, a crash) does not
        // always end the stream cleanly: the JDK closes a dead child's
        // pipes, a peer resets its socket, and the WRITE throws "Stream
        // closed" or "Broken pipe" (supervised-crash-every-language)
        live = false
        throw IllegalStateException(s"$who is DEAD (its wire broke: ${e.getMessage}) — a supervisor retry gets a fresh one")
    answer.getOrElse {
      live = false
      throw IllegalStateException(s"$who is DEAD (eof on the wire) — a supervisor retry gets a fresh one")
    }

  /**
   * Presence and version of named packages (the shim's `verify`),
   * mismatches as data naming the package — the wrong environment becomes
   * a loud refusal at start instead of a subtly different answer later.
   */
  def verify(packages: Map[String, String]): Vector[String] =
    val asked = Json.JArr(packages.keys.toVector.sorted.map(Json.JStr(_)))
    WireSession.answer(exchange(Json.JObj(Vector("op" -> Json.JStr("verify"), "packages" -> asked))))(
      (k, m) => s"$k: $m")(Right(_)) match
      case Left(why) => Vector(s"verify itself failed: $why")
      case Right(Json.JObj(fs)) =>
        val have = fs.toMap.get("packages") match
          case Some(Json.JObj(ps)) => ps.toMap
          case _ => Map.empty[String, Json]
        packages.toVector.sortBy(_._1).flatMap { (name, want) =>
          WireSession.unboxed(have.get(name)).collect { case Json.JStr(v) => v } match
            case None => Some(s"package '$name' is MISSING (wanted $want)")
            case Some(v) if !v.startsWith(want) => Some(s"package '$name' is $v, wanted $want")
            case _ => None
        }
      case Right(other) => Vector(s"verify answered strangely: $other")

  def close(): Unit =
    live = false
    link.close()
    if deadline.isDefined then reader.shutdownNow(): Unit

object WireSession:

  /** an answer that did not come within the deadline; the message says DEAD
   * so a supervisor that retires dead workers (`PyWorkers`) retires it */
  final class TimedOut(val millis: Long) extends IllegalStateException(
    s"the worker is DEAD: no answer within ${millis}ms, so its wire was closed — a supervisor gets a fresh one")

  /**
   * The session over any link: the far side speaks first, and its hello
   * decides the rest. Refused BY NAME, the link closed: a hello that never
   * came, one that names its own `fatal` (a prerequisite the far side
   * lacks), a shim of another version than `shimVersion`, a failed auth,
   * a wire the givens ask for and the far side did not announce, and a
   * deadline on a link whose calls cannot be abandoned.
   *
   * @param name       who the far side is, in a refusal: "'python3'", "the worker at h:p"
   * @param versionKey the hello's key for the far side's own version: "python", "r"
   */
  def over(link: WireLink, name: String, shimVersion: Int, versionKey: String, who: String)
          (using format: WireFormat, compression: WireCompression, auth: WireAuth,
           deadline: WireDeadline)(using frames: okay.codec.FrameFormat): WireSession =
    def refuse(why: String): Nothing =
      link.close()
      throw IllegalStateException(why)
    if link.inProcess && deadline.millis.isDefined then
      refuse(s"$name is in this process: a call into it runs on the caller's thread and cannot be abandoned, " +
        "so a given WireDeadline cannot be kept here — refused rather than promised")
    val hello = whole(link.hello().getOrElse(refuse(s"$name answered nothing (stderr may know)")))
    val fields = hello match
      case Json.JObj(fs) => fs.toMap
      case _ => Map.empty[String, Json]
    // a far side that cannot run says why, in its hello (R without jsonlite)
    unboxed(fields.get("fatal")).collect { case Json.JStr(why) => why }.foreach(refuse)
    val shimV = unboxed(fields.get("shim")).collect { case Json.JNum(n) => n.toInt }.getOrElse(-1)
    if shimV != shimVersion then
      refuse(s"shim/host version drift: $name says v$shimV, this host speaks v$shimVersion — refuse rather than guess")
    val version = unboxed(fields.get(versionKey)).collect { case Json.JStr(s) => s }.getOrElse("?")
    // stage 5b: who may speak, before what the wire looks like
    WireNegotiation.authenticate(hello, name, line => link.roundTrip(line).map(whole)).left.foreach(refuse)
    val (codec, arrow) = configure(link, name, hello).fold(refuse, identity)
    val columnar = fields.get("speaks") match
      case Some(Json.JObj(sp)) => sp.toMap.get("frames") match
        case Some(Json.JArr(xs)) => xs.contains(Json.JStr("columnar"))
        case Some(Json.JStr(x)) => x == "columnar"
        case _ => false
      case _ => false
    // a link that carries a table as itself takes the table road whatever
    // the far side's hello said about Arrow streams (foreign-arrow-ffm)
    new WireSession(link, version, codec, deadline.millis, arrow || link.tables.isDefined, frames, who, columnar)

  /** stage 5's handshake (`okay.codec.WireNegotiation`): what the givens
   * ask for, checked against what the far side announced, and confirmed */
  private def configure(link: WireLink, name: String, hello: Json)
                       (using WireFormat, WireCompression, okay.codec.FrameFormat)
      : Either[String, (Option[(WireFormat, WireCompression)], Boolean)] =
    for
      arrow <- WireNegotiation.chooseFrames(hello, name)
      chosen <- WireNegotiation.choose(hello, link.network, name)
      // Arrow needs frames on the wire, so it configures even json/none
      codec = chosen match
        case None if arrow => Some((WireFormat.json, WireCompression.Off.off))
        case other => other
      _ <- codec match
        case None => Right(())
        case Some((f, c)) =>
          WireNegotiation.confirmed(name, f, c, link.roundTrip(WireNegotiation.configure(f, c, arrow)).map(whole))
    yield (codec, arrow)

  /** a wire line, read strictly (`okay.codec.WireJson.whole`) */
  def whole(line: String): Json = okay.codec.WireJson.whole(line)

  /** jsonlite UNBOXES a length-1 vector, and may leave one boxed: a scalar
   * field is read either way, for every far side */
  def unboxed(j: Option[Json]): Option[Json] = j match
    case Some(Json.JArr(Vector(one))) => Some(one)
    case other => other

  /**
   * An answer: `{"ok": v}` read by `ok`, or `{"condition": {kind,
   * message}}` made into the language's own refusal by `condition`. A
   * message R sends as a character vector of several lines is joined.
   */
  def answer[C, A](j: Json)(condition: (String, String) => C)(ok: Json => Either[C, A]): Either[C, A] =
    j match
      case Json.JObj(fs) =>
        val m = fs.toMap
        m.get("condition") match
          case Some(Json.JObj(c)) =>
            val cm = c.toMap
            def str(k: String) = cm.get(k).collect {
              case Json.JStr(s) => s
              case Json.JArr(xs) => xs.collect { case Json.JStr(s) => s }.mkString("\n")
            }.getOrElse("")
            Left(condition(str("kind"), str("message")))
          case _ => m.get("ok") match
            case Some(v) => ok(v)
            case None => Left(condition("WireError", s"no ok and no condition in $j"))
      case other => Left(condition("WireError", s"not an answer: $other"))

  /** an executable named relative to PATH, resolved HERE: a worker's
   * environment is clean, so it cannot resolve its own */
  def resolve(exe: String): String =
    if exe.contains("/") then exe
    else
      sys.env.getOrElse("PATH", "").split(":").iterator
        .map(d => java.nio.file.Paths.get(d, exe))
        .find(p => java.nio.file.Files.isExecutable(p))
        .map(_.toString)
        .getOrElse(exe)   // let the start produce the loud refusal

  /** a resource of `owner`'s jar (a shim) as a file a process can run */
  def shipped(owner: Class[?], resource: String, prefix: String, suffix: String): java.nio.file.Path =
    val f = java.nio.file.Files.createTempFile(prefix, suffix)
    val res = owner.getResourceAsStream(resource)
    if res == null then throw IllegalStateException(s"the resource $resource is missing from the jar")
    try java.nio.file.Files.copy(res, f, java.nio.file.StandardCopyOption.REPLACE_EXISTING)
    finally res.close()
    f.toFile.deleteOnExit()
    f
