package okay.audit

import java.nio.file.{Files, Path, Paths}

/** The standalone audit manifest. This is deliberately the small schema the
  * command accepts, rather than a general JSON model: no dependency belongs
  * on an audit tool's runtime classpath. */
final case class Manifest(layers: Map[String, Layer], modules: Map[String, Seq[Path]],
                          packages: Map[String, Map[String, Layer]], allows: Vector[Allow])

object Manifest:
  private enum Token:
    case OpenObject, CloseObject, OpenArray, CloseArray, Colon, Comma, End
    case Text(value: String)

  private final class Lexer(text: String):
    private var at = 0

    def next(): Token =
      while at < text.length && text.charAt(at).isWhitespace do at += 1
      if at == text.length then Token.End
      else
        val c = text.charAt(at); at += 1
        c match
          case '{' => Token.OpenObject
          case '}' => Token.CloseObject
          case '[' => Token.OpenArray
          case ']' => Token.CloseArray
          case ':' => Token.Colon
          case ',' => Token.Comma
          case '"' => string()
          case _ => refuse(s"unexpected '$c' at character ${at - 1}; strings must be quoted")

    private def string(): Token =
      val out = StringBuilder()
      var closed = false
      while at < text.length && !closed do
        val c = text.charAt(at); at += 1
        c match
          case '"' => closed = true
          case '\\' =>
            if at == text.length then refuse("unfinished escape at end of input")
            val escaped = text.charAt(at); at += 1
            escaped match
              case '"' => out += '"'
              case '\\' => out += '\\'
              case '/' => out += '/'
              case 'b' => out += '\b'
              case 'f' => out += '\f'
              case 'n' => out += '\n'
              case 'r' => out += '\r'
              case 't' => out += '\t'
              case 'u' =>
                if at + 4 > text.length then refuse("short unicode escape")
                val hex = text.substring(at, at + 4)
                try out += Integer.parseInt(hex, 16).toChar
                catch case _: NumberFormatException => refuse(s"bad unicode escape \\u$hex")
                at += 4
              case other => refuse(s"unknown escape \\$other")
          case control if control < ' ' => refuse("control character in string")
          case other => out += other
      if !closed then refuse("unterminated string")
      Token.Text(out.result())

  private final class Parser(text: String, base: Path):
    private val lexer = Lexer(text)
    private var token = lexer.next()

    def manifest(): Manifest =
      openObject("manifest")
      var modules: Option[Vector[(String, Layer, Seq[Path], Map[String, Layer])]] = None
      var allows: Option[Vector[Allow]] = None
      while token != Token.CloseObject do
        val key = string("manifest field")
        colon(key)
        key match
          case "modules" =>
            if modules.nonEmpty then refuse("manifest names modules twice")
            modules = Some(moduleArray())
          case "allows" => allows = once(allows, "allows", allowArray())
          case other => refuse(s"manifest has unknown field '$other'")
        separator("manifest")
      take()
      if token != Token.End then refuse("text after manifest")
      val rows = modules.getOrElse(refuse("manifest has no modules"))
      val names = rows.map(_._1)
      names.groupBy(identity).collectFirst { case (name, xs) if xs.size > 1 => name }
        .foreach(name => refuse(s"manifest names module '$name' more than once"))
      Manifest(rows.map(r => r._1 -> r._2).toMap, rows.map(r => r._1 -> r._3).toMap,
        rows.map(r => r._1 -> r._4).toMap, allows.getOrElse(Vector.empty))

    private def moduleArray(): Vector[(String, Layer, Seq[Path], Map[String, Layer])] =
      openArray("modules")
      val out = Vector.newBuilder[(String, Layer, Seq[Path], Map[String, Layer])]
      while token != Token.CloseArray do
        out += module()
        separator("modules")
      take(); out.result()

    private def module(): (String, Layer, Seq[Path], Map[String, Layer]) =
      openObject("module")
      var name: Option[String] = None
      var layer: Option[Layer] = None
      var paths: Option[Seq[Path]] = None
      var packages: Option[Map[String, Layer]] = None
      while token != Token.CloseObject do
        val key = string("module field"); colon(key)
        key match
          case "name" => name = once(name, "name", string("module name"))
          case "layer" => layer = once(layer, "layer", layerOf(string("module layer"), "module layer"))
          case "paths" => paths = once(paths, "paths", pathArray())
          case "packages" => packages = once(packages, "packages", packageObject())
          case other => refuse(s"module has unknown field '$other'")
        separator("module")
      take()
      val n = name.getOrElse(refuse("module has no name"))
      val ps = paths.getOrElse(refuse(s"module '$n' has no paths"))
      if ps.isEmpty then refuse(s"module '$n' has no paths")
      (n, layer.getOrElse(refuse(s"module '$n' has no layer")), ps, packages.getOrElse(Map.empty))

    private def pathArray(): Seq[Path] =
      openArray("paths")
      val out = Vector.newBuilder[Path]
      while token != Token.CloseArray do
        val raw = string("path")
        if raw.trim.isEmpty then refuse("path is empty")
        val path = Paths.get(raw)
        val resolved = if path.isAbsolute then path.normalize else base.resolve(path).normalize
        if !Files.exists(resolved) then refuse(s"path does not exist: $resolved")
        out += resolved
        separator("paths")
      take(); out.result()

    private def packageObject(): Map[String, Layer] =
      openObject("packages")
      val out = scala.collection.mutable.Map.empty[String, Layer]
      while token != Token.CloseObject do
        val prefix = string("package prefix")
        if prefix.trim.isEmpty then refuse("package prefix is empty")
        if out.contains(prefix) then refuse(s"package prefix '$prefix' appears twice")
        colon(prefix)
        out(prefix) = layerOf(string("package layer"), s"package '$prefix' layer")
        separator("packages")
      take(); out.toMap

    private def allowArray(): Vector[Allow] =
      openArray("allows")
      val out = Vector.newBuilder[Allow]
      while token != Token.CloseArray do
        openObject("allow")
        var module: Option[String] = None
        var api: Option[String] = None
        var owner: Option[String] = None
        var reason: Option[String] = None
        while token != Token.CloseObject do
          val key = string("allow field"); colon(key)
          key match
            case "module" => module = once(module, "module", string("allow module"))
            case "api" => api = once(api, "api", string("allow api"))
            case "owner" => owner = once(owner, "owner", string("allow owner"))
            case "reason" => reason = once(reason, "reason", string("allow reason"))
            case other => refuse(s"allow has unknown field '$other'")
          separator("allow")
        take()
        out += Allow(module.getOrElse(refuse("allow has no module")), api.getOrElse(refuse("allow has no api")),
          reason.getOrElse(refuse("allow has no reason")), owner.getOrElse(refuse("allow has no owner")))
        separator("allows")
      take(); out.result()

    private def layerOf(raw: String, where: String): Layer = raw match
      case "business" => Layer.Business
      case "handlers" => Layer.Handlers
      case "runtime" => Layer.Runtime
      case "untracked" => Layer.Untracked
      case other => refuse(s"$where is '$other', not business, handlers, runtime or untracked")

    private def once[A](old: Option[A], field: String, value: A): Option[A] =
      if old.nonEmpty then refuse(s"object names $field twice")
      Some(value)

    private def openObject(where: String): Unit = expect(Token.OpenObject, s"$where must be an object")
    private def openArray(where: String): Unit = expect(Token.OpenArray, s"$where must be an array")
    private def colon(where: String): Unit = expect(Token.Colon, s"$where needs ':'")
    private def string(where: String): String = token match
      case Token.Text(value) => take(); value
      case _ => refuse(s"$where must be a string")

    /** Consume either a comma and another value, or leave the closing token. */
    private def separator(where: String): Unit = token match
      case Token.Comma =>
        take()
        if token == Token.CloseObject || token == Token.CloseArray then refuse(s"$where has a trailing comma")
      case Token.CloseObject | Token.CloseArray =>
      case _ => refuse(s"$where needs ',' or its closing delimiter")

    private def expect(expected: Token, message: String): Unit =
      if token != expected then refuse(message)
      take()
    private def take(): Unit = token = lexer.next()

  def read(file: Path): Manifest =
    val absolute = file.toAbsolutePath.normalize
    if !Files.isRegularFile(absolute) then refuse(s"manifest is not a file: $absolute")
    Parser(Files.readString(absolute), Option(absolute.getParent).getOrElse(Paths.get(".").toAbsolutePath)).manifest()

  private def refuse(message: String): Nothing = throw Audit.Refused(s"manifest: $message")
