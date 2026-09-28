package okay.dlm.remote

import okay.codec.Json
import okay.codec.Json.*
import okay.dlm.Embedder

/**
 * A REMOTE ENCODER over the OpenAI-shaped embeddings wire, which is
 * the one every hosted encoder speaks and every local server (Ollama,
 * vLLM, TEI) mimics:
 *
 *   POST {base}/v1/embeddings  {"model": …, "input": "text"}
 *   → {"data": [{"embedding": [..floats..]}]}
 *
 * The `Embedder` seam is what it plugs into, under a name made of the
 * host and the model — so a table compiled against one server's
 * MiniLM is refused by name against another's, exactly as the local
 * artifacts are.
 *
 * A wire that fails cannot embed, and an encoder that cannot embed
 * has no honest vector to return: it THROWS, and the caller decides
 * — a build step stops, a request path that wants to survive wraps
 * the router's judge in `Judge.guarded` and keeps its rules.
 */
object Embeddings:

  final case class Config(base: String, model: String, apiKey: Option[String] = None,
                          path: String = "/v1/embeddings", dim: Int = 0)

  def encode(model: String, text: String): Json =
    JObj(Vector("model" -> JStr(model), "input" -> JStr(text)))

  def decode(raw: String): Either[String, okay.rag.Embedding] =
    val j = try Json.parse(raw) catch case e: Exception => JStr(s"broken: ${e.getMessage}")
    j match
      case JObj(fs) =>
        fs.collectFirst { case ("data", JArr(ds)) => ds }.flatMap(_.headOption).flatMap {
          case JObj(d) => d.collectFirst { case ("embedding", JArr(xs)) =>
            okay.rag.embedding(xs.collect { case JNum(v) => v.toFloat }.toArray) }
          case _ => None
        }.toRight(s"no data[0].embedding in ${raw.take(120)}")
      case _ => Left(s"not a JSON object: ${raw.take(120)}")

  /** the encoder; `dim` is read off the first vector when the config
   * does not say */
  def openAi(config: Config)(using wire: Wire): Embedder = new Embedder:
    val name = s"${config.base.stripSuffix("/").stripPrefix("https://").stripPrefix("http://")}/${config.model}"
    private val headers = Map("Content-Type" -> "application/json") ++
      config.apiKey.map(k => "Authorization" -> s"Bearer $k")
    @volatile private var seen = config.dim
    def dim: Int = seen
    def apply(text: String): okay.rag.Embedding =
      wire.post(config.base.stripSuffix("/") + config.path, headers, Json.print(encode(config.model, text)))
        .flatMap(decode) match
        case Right(v) => if seen == 0 then seen = v.length; v
        case Left(why) => throw IllegalStateException(s"$name: $why")

  def openAi(base: String, model: String, apiKey: Option[String] = None)(using Wire): Embedder =
    openAi(Config(base, model, apiKey))
