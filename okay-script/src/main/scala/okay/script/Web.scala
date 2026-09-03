package okay.script

/** The incoming HTTP request a `render`-mode script is answering, as
 * plain, dependency-free data -- no `okay.http.Request` import here:
 * `okay-script`'s only dependency stays `scala3-compiler`. A caller
 * (an `okay-jetty` route) translates its OWN `Request` into this
 * before calling `render`/`Page.render`. See specs/okay-script.md
 * "Request context".
 *
 * Unlike `Meta.current`, NOT auto-injected by the tokenizer -- there
 * is exactly one request per render call, so a script just imports
 * `okay.script.Web` and reads `Web.current` wherever it needs it.
 */
final case class Web(
  method: String,
  path: String,
  query: Map[String, String] = Map.empty,
  headers: Map[String, String] = Map.empty,
)

object Web:
  val empty: Web = Web("GET", "/")

  @volatile private var _current: Web = empty

  def current: Web = _current

  def setCurrent(w: Web): Unit = _current = w

  // A host-side `Web` cannot be handed directly to a script running in
  // an ISOLATED classloader (okay-script-classloader-isolation): the
  // isolated loader compiles/loads its OWN separate copy of THIS
  // class (same bytecode, different Class identity), so passing a
  // host-built instance across via reflection fails with an argument
  // type mismatch -- the same trap `Compiled.invoke`'s `scala.Console`
  // fix already found, here for a user-defined type instead of a JDK
  // one. `String`/`Array[String]` cross the boundary safely (bootstrap
  // classes), so `ScalaScript.compileOnly` encodes `Web.current` into
  // a flat `Array[String]` on the HOST side and passes it as the
  // compiled program's own `args`; the ISOLATED classloader's copy of
  // `Web` decodes it back into ITS OWN `Web` instance, entirely within
  // its own classloader -- no cross-boundary object ever crosses.
  def encodeArgs(w: Web): Array[String] =
    val q = w.query.toVector
    val h = w.headers.toVector
    (Vector(w.method, w.path, q.size.toString)
      ++ q.flatMap((k, v) => Vector(k, v))
      ++ Vector(h.size.toString)
      ++ h.flatMap((k, v) => Vector(k, v))).toArray

  def decodeArgs(args: Array[String]): Web =
    if args.length < 3 then empty
    else
      val method = args(0)
      val path = args(1)
      val qn = args(2).toInt
      val qStart = 3
      val query = (0 until qn).map(i => args(qStart + 2 * i) -> args(qStart + 2 * i + 1)).toMap
      val hStart = qStart + 2 * qn
      val hn = args(hStart).toInt
      val hStartPairs = hStart + 1
      val headers = (0 until hn).map(i => args(hStartPairs + 2 * i) -> args(hStartPairs + 2 * i + 1)).toMap
      Web(method, path, query, headers)
