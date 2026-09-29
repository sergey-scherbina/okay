package okay.llm

import okay.{!, %, +, Async, Writer, effect, pure}
import okay.codec.Json
import okay.codec.Json.*

/**
 * THE MODEL CATALOG, RESIDENCY AND STORE — as capabilities a provider
 * may LACK (specs/llm-models.md).
 *
 * No single standard covers "which models exist, which one is loaded,
 * load another, pull, remove": hosted providers have nothing to load. Two
 * de-facto standards cover it between them — the catalog is OpenAI's
 * `GET /v1/models` shape, which everyone returns; the lifecycle verbs are
 * Ollama's (`ps`/`pull`/`rm`, load/unload), which the local runtimes
 * converged on. The seam is their union, each part a trait, and an
 * adapter's TYPE says what it can do: `openAi(…)` is a `Catalog` and a
 * call to `.load` on it does not compile. A screen drawn from the type
 * has no button that answers "not supported".
 */

/** the reads and the one delete the catalog needs, beside `Transport`'s
 * post; the two real transports (JVM, JS) carry both, a test writes the
 * one it needs */
trait Fetch:
  def get(url: String, headers: Map[String, String]): Unit ! Writer % String + Async
  def delete(url: String, headers: Map[String, String], body: String): Unit ! Writer % String + Async

/**
 * One model, several spellings, one identity: `org:repo`, `org/repo`
 * and `hf:org/repo` name one set of weights (a string compare on this
 * path once warmed a second resident copy in rozum). Equality and
 * hashing are by the canonical `key`; `spelling` is what the provider
 * said and what goes back to it — Ollama's `name:tag` must not come
 * back as `name/tag`.
 */
final class ModelId(val spelling: String):
  val key: String = ModelId.key(spelling)
  override def equals(o: Any): Boolean = o match
    case m: ModelId => m.key == key
    case _ => false
  override def hashCode: Int = key.hashCode
  override def toString: String = spelling

object ModelId:
  def apply(s: String): ModelId = new ModelId(s.trim)
  /** the canonical `org/repo` form */
  def show(id: ModelId): String = id.key
  def same(a: String, b: String): Boolean = key(a) == key(b)
  private def key(s: String): String =
    val t = s.trim
    (if t.startsWith("hf:") then t.drop(3) else t).replace(':', '/')

object Models:

  /** one model as a catalog names it */
  final case class Model(id: ModelId, owner: Option[String], created: Option[Long],
                         contextTokens: Option[Int], maxOutput: Option[Int],
                         capabilities: Set[String])
  final case class Resident(id: ModelId, memoryBytes: Option[Long], expiresAt: Option[Long])
  final case class Weights(id: ModelId, bytes: Option[Long], digest: Option[String])
  enum Progress:
    case Bytes(done: Long, total: Option[Long])
    case Done
    case Failed(reason: String)

  /** one call refused, as the host said it: the method, a code, its words */
  final case class Refused(method: String, code: Int, description: String)

  /** what every provider has */
  trait Catalog:
    def list: Vector[Model] ! Async
    def info(id: ModelId): Option[Model] ! Async = list.map(_.find(_.id == id))
  /** what a host with memory has */
  trait Residency:
    def running: Vector[Resident] ! Async
    def load(id: ModelId): Either[Refused, Unit] ! Async
    def unload(id: ModelId): Either[Refused, Unit] ! Async
  /** what a host with a disk has */
  trait Store:
    def local: Vector[Weights] ! Async
    def pull(id: ModelId): Unit ! Writer % Progress + Async
    def remove(id: ModelId): Either[Refused, Unit] ! Async

  // ---- reading a body ------------------------------------------------------

  private type Lines = Unit ! Writer % String + Async

  private def body(lines: Lines): Json ! Async =
    Writer.run[String, Unit, Async](lines).map((ls, _) => Json.parse(ls.mkString("\n")))

  private[llm] object J:
    def field(j: Json, k: String): Option[Json] = j match
      case JObj(fs) => fs.collectFirst { case (`k`, v) => v }
      case _ => None
    def str(j: Json, k: String): Option[String] = field(j, k).collect { case JStr(s) => s }
    def num(j: Json, k: String): Option[Double] = field(j, k).collect { case JNum(n) => n }
    def long(j: Json, k: String): Option[Long] = num(j, k).map(_.toLong)
    def bool(j: Json, k: String): Boolean = field(j, k).contains(JBool(true))
    def arr(j: Json, k: String): Vector[Json] = field(j, k) match
      case Some(JArr(vs)) => vs
      case _ => Vector.empty
    def strings(j: Json, k: String): Set[String] = field(j, k) match
      case Some(JArr(vs)) => vs.collect { case JStr(s) => s }.toSet
      case Some(JObj(fs)) => fs.collect { case (n, JBool(true)) => n }.toSet
      case _ => Set.empty

  /** the OpenAI form: `{object:"list", data:[{id, owned_by, created}]}`,
   * plus what Anthropic adds where present */
  private def openAiRow(j: Json): Model =
    Model(ModelId(J.str(j, "id").getOrElse("")), J.str(j, "owned_by"), J.long(j, "created"),
      J.long(j, "max_input_tokens").map(_.toInt), J.long(j, "max_tokens").map(_.toInt),
      J.strings(j, "capabilities"))

  /** a refusal read off a body: `{error:{message,type}}` (OpenAI form,
   * rozum), `{error:"…"}` (Ollama), or a body that is not the shape asked */
  private def refusal(method: String, j: Json): Option[Refused] =
    J.field(j, "error").map {
      case JStr(s) => Refused(method, 0, s)
      case e => Refused(method, J.long(e, "code").map(_.toInt).getOrElse(0), J.str(e, "message").getOrElse(Json.print(e).take(200)))
    }

  private def okOr(method: String, j: Json, ok: Json => Boolean): Either[Refused, Unit] =
    refusal(method, j) match
      case Some(r) => Left(r)
      case None if ok(j) => Right(())
      case None => Left(Refused(method, 0, Json.print(j).take(200)))

  // ---- adapters ------------------------------------------------------------

  private def bearer(key: String) = Map("authorization" -> s"Bearer $key", "accept" -> "application/json")
  private val json = Map("content-type" -> "application/json", "accept" -> "application/json")

  /** OpenAI, and every OpenAI-form server that lists but cannot load */
  def openAi(fetch: Fetch, apiKey: String, base: String = "https://api.openai.com"): Catalog = new:
    def list: Vector[Model] ! Async =
      body(fetch.get(s"$base/v1/models", bearer(apiKey))).map(j => J.arr(j, "data").map(openAiRow))

  /** Anthropic: the same list with the limits and capabilities, paginated */
  def anthropic(fetch: Fetch, apiKey: String, base: String = "https://api.anthropic.com"): Catalog = new:
    private val headers = Map("x-api-key" -> apiKey, "anthropic-version" -> "2023-06-01", "accept" -> "application/json")
    def list: Vector[Model] ! Async =
      def page(after: Option[String], acc: Vector[Model], left: Int): Vector[Model] ! Async =
        val url = s"$base/v1/models?limit=100" + after.fold("")(a => s"&after_id=$a")
        body(fetch.get(url, headers)).flatMap { j =>
          val rows = J.arr(j, "data").map(openAiRow)
          val more = J.bool(j, "has_more") && left > 0
          J.str(j, "last_id").filter(_ => more) match
            case Some(last) => page(Some(last), acc ++ rows, left - 1)
            case None => pure(acc ++ rows)
        }
      page(None, Vector.empty, 50)   // BOUNDED: fifty pages of a hundred, a catalog is not that long

  /** Ollama: the catalog in OpenAI form, the lifecycle in its own verbs */
  def ollama(transport: Transport & Fetch, base: String = "http://127.0.0.1:11434"): Catalog & Residency & Store = new Catalog with Residency with Store:
    def list: Vector[Model] ! Async =
      body(transport.get(s"$base/v1/models", json)).map(j => J.arr(j, "data").map(openAiRow))
    def local: Vector[Weights] ! Async =
      body(transport.get(s"$base/api/tags", json)).map(j => J.arr(j, "models").map(m =>
        Weights(ModelId(J.str(m, "model").orElse(J.str(m, "name")).getOrElse("")), J.long(m, "size"), J.str(m, "digest"))))
    def running: Vector[Resident] ! Async =
      body(transport.get(s"$base/api/ps", json)).map(j => J.arr(j, "models").map(m =>
        Resident(ModelId(J.str(m, "model").orElse(J.str(m, "name")).getOrElse("")), J.long(m, "size_vram").orElse(J.long(m, "size")),
          J.str(m, "expires_at").flatMap(Iso.epochMs))))
    /** load is a request with no prompt; unload the same with `keep_alive: 0` */
    def load(id: ModelId): Either[Refused, Unit] ! Async = keep(id, JStr("5m"))
    def unload(id: ModelId): Either[Refused, Unit] ! Async = keep(id, JNum(0))
    private def keep(id: ModelId, alive: Json): Either[Refused, Unit] ! Async =
      body(transport.post(s"$base/api/generate", json,
        Json.print(JObj(Vector("model" -> JStr(id.spelling), "keep_alive" -> alive)))))
        .map(j => okOr("generate", j, j => J.bool(j, "done") || J.str(j, "model").isDefined))
    def pull(id: ModelId): Unit ! Writer % Progress + Async =
      type F = Writer % Progress + Async
      val lines = transport.post(s"$base/api/pull", json, Json.print(JObj(Vector("model" -> JStr(id.spelling), "stream" -> JBool(true)))))
      def eventOf(l: String): Option[Progress] =
        val j = Json.parse(l)
        J.str(j, "error").map(Progress.Failed(_))
          .orElse(if J.str(j, "status").contains("success") then Some(Progress.Done) else None)
          .orElse(J.long(j, "completed").map(c => Progress.Bytes(c, J.long(j, "total"))))
      okay.!.widen[(Seq[String], Unit), Async, Writer % Progress](Writer.run[String, Unit, Async](lines)).flatMap { (ls, _) =>
        // the recursion is only through the program's flatMap: trampolined
        def go(rest: List[Progress]): Unit ! F = rest match
          case Nil => pure(())
          case e :: t => effect[F, Unit](Writer(e)).flatMap(_ => go(t))
        go(ls.iterator.flatMap(eventOf).toList)
      }
    def remove(id: ModelId): Either[Refused, Unit] ! Async =
      body(transport.delete(s"$base/api/delete", json, Json.print(JObj(Vector("model" -> JStr(id.spelling))))))
        .map(j => refusal("delete", j).toLeft(()))

  /**
   * rozum: the catalog in OpenAI form with the resident row marked, the
   * residency through the gateway's control routes. No store: weights are
   * pulled and removed by the `rozum models` CLI, which has no route yet,
   * so the type says `Catalog & Residency` and nothing more.
   *
   * The resident row's `id` is a Claude-shaped alias for clients whose
   * pickers filter on the name; its real spec rides in `display_name`,
   * which is what this adapter calls the model.
   */
  def rozum(transport: Transport & Fetch, base: String): Catalog & Residency = new Catalog with Residency:
    private def rows: Vector[Json] ! Async = body(transport.get(s"$base/v1/models", json)).map(J.arr(_, "data"))
    private def idOf(j: Json) = ModelId(J.str(j, "display_name").orElse(J.str(j, "id")).getOrElse(""))
    def list: Vector[Model] ! Async = rows.map(_.map { j =>
      openAiRow(j).copy(id = idOf(j), capabilities = if J.bool(j, "resident") then Set("resident") else Set.empty) })
    def running: Vector[Resident] ! Async =
      rows.map(_.filter(J.bool(_, "resident")).map(j => Resident(idOf(j), J.long(j, "size_bytes"), None)))
    def load(id: ModelId): Either[Refused, Unit] ! Async =
      body(transport.post(s"$base/control/switch", json, Json.print(JObj(Vector("model" -> JStr(id.spelling))))))
        .map(j => okOr("switch", j, J.str(_, "status").contains("switched")))
    def unload(id: ModelId): Either[Refused, Unit] ! Async =
      body(transport.post(s"$base/control/unload", json, "{}"))
        .map(j => okOr("unload", j, J.str(_, "status").contains("unloaded")))

  /** `2024-06-04T14:38:31.83753-07:00` to epoch milliseconds, on every
   * platform: no java.time on JS. Total: anything else is None. */
  private[llm] object Iso:
    def epochMs(s: String): Option[Long] =
      val re = """(\d{4})-(\d{2})-(\d{2})[Tt ](\d{2}):(\d{2}):(\d{2})(?:\.(\d+))?(Z|z|([+-])(\d{2}):?(\d{2}))?""".r
      s.trim match
        case re(y, mo, d, h, mi, sec, frac, zone, sign, zh, zm) =>
          val days = civilDays(y.toInt, mo.toInt, d.toInt)
          val ms = if frac == null then 0L else (frac + "000").take(3).toLong
          val offset = if zone == null || zone.equalsIgnoreCase("z") then 0L
            else (if sign == "-" then -1L else 1L) * (zh.toLong * 3600 + zm.toLong * 60) * 1000
          Some(((days * 86400L + h.toLong * 3600 + mi.toLong * 60 + sec.toLong) * 1000 + ms) - offset)
        case _ => None
    /** days since 1970-01-01 (Howard Hinnant's days_from_civil) */
    private def civilDays(y0: Int, m: Int, d: Int): Long =
      val y = if m <= 2 then y0 - 1 else y0
      val era = (if y >= 0 then y else y - 399) / 400
      val yoe = y - era * 400
      val doy = (153 * (if m > 2 then m - 3 else m + 9) + 2) / 5 + d - 1
      val doe = yoe * 365 + yoe / 4 - yoe / 100 + doy
      era * 146097L + doe - 719468L
