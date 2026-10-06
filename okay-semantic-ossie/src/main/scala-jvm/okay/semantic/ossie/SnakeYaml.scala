package okay.semantic.ossie

import okay.codec.Json
import Json.*
import scala.jdk.CollectionConverters.*
import org.snakeyaml.engine.v2.api.LoadSettings
import org.snakeyaml.engine.v2.api.lowlevel.{Compose, Parse}
import org.snakeyaml.engine.v2.events.Event.ID
import org.snakeyaml.engine.v2.nodes.{Node, ScalarNode, SequenceNode, MappingNode, Tag}

/** Optional YAML interpreter; the portable module never names SnakeYAML. */
object SnakeYaml extends Syntax:
  given Syntax = this
  def missing(loader: ClassLoader = getClass.getClassLoader): Option[String] =
    scala.util.Try(Class.forName("org.snakeyaml.engine.v2.api.LoadSettings",false,loader)).failed.toOption
      .map(_ => "SnakeYaml requires optional org.snakeyaml:snakeyaml-engine")
  def byName(name: String): Either[String,Syntax] =
    if name != "snakeyaml" then Syntax.byName(name)
    else missing().toLeft(this)
  def readJson(text: String): Either[Vector[String],Json] = PortableSyntax.readJson(text)
  def writeJson(value: Json): String = PortableSyntax.writeJson(value)
  def readYaml(text: String): Either[Vector[String],Json] = missing() match
    case Some(why) => Left(Vector(why))
    case None => scala.util.Try {
      val settings = LoadSettings.builder().setAllowDuplicateKeys(false).setMaxAliasesForCollections(0)
        .setCodePointLimit(1000000).build()
      // Compose recurses in the dependency. The event pre-pass enforces depth <= 128.
      var depth = 0
      var documents = 0
      val events = new Parse(settings).parseString(text).iterator()
      while events.hasNext do
        events.next().getEventId match
          case ID.MappingStart | ID.SequenceStart =>
            depth += 1
            require(depth <= 128,"YAML nesting exceeds 128")
          case ID.MappingEnd | ID.SequenceEnd => depth -= 1
          case ID.Alias => throw IllegalArgumentException("YAML aliases are not supported")
          case ID.DocumentStart =>
            documents += 1
            require(documents <= 1,"expected exactly one YAML document")
          case _ => ()
      val composed = new Compose(settings).composeString(text)
      if composed.isEmpty then Left(Vector("empty YAML document")) else project(composed.get())
    }.toEither.left.map(e => Vector(s"YAML: ${e.getMessage}")).flatMap(identity)

  private enum Task:
    case Visit(node: Node)
    case ArrayDone(size: Int)
    case ObjectDone(keys: Vector[String])
  private def project(root: Node): Either[Vector[String],Json] =
    var work = List(Task.Visit(root))
    var done = List.empty[Json]
    val errors = Vector.newBuilder[String]
    while work.nonEmpty do
      val next = work.head; work = work.tail
      next match
        case Task.Visit(node) => node match
          case s: ScalarNode =>
            val value = s.getValue
            val tag = s.getTag
            val converted =
              if tag == Tag.STR then Right(JStr(value))
              else if tag == Tag.NULL then
                if value.isEmpty || value == "~" || value.equalsIgnoreCase("null") then Right(JNull) else Left(s"invalid YAML null $value")
              else if tag == Tag.BOOL then
                if value.equalsIgnoreCase("true") || value.equalsIgnoreCase("false") then Right(JBool(value.equalsIgnoreCase("true"))) else Left(s"invalid YAML bool $value")
              else if tag == Tag.INT then
                scala.util.Try(BigDecimal(value)).toOption match
                  case Some(n) if n.isWhole => Exact.number(value)
                  case _ => Left(s"invalid YAML integer $value")
              else if tag == Tag.FLOAT then Exact.number(value)
              else Left(s"unsupported YAML tag $tag")
            converted match
              case Right(v) => done = v :: done
              case Left(why) => errors += why; done = JNull :: done
          case s: SequenceNode =>
            if s.getTag != Tag.SEQ then errors += s"unsupported YAML tag ${s.getTag}"
            val children = s.getValue.asScala.toVector
            work = children.map(Task.Visit.apply).toList ::: Task.ArrayDone(children.size) :: work
          case m: MappingNode =>
            if m.getTag != Tag.MAP then errors += s"unsupported YAML tag ${m.getTag}"
            val tuples = m.getValue.asScala.toVector
            val keys = tuples.map(_.getKeyNode).map {
              case s: ScalarNode if s.getTag == Tag.STR => s.getValue
              case _ => errors += "YAML mapping keys must be strings"; ""
            }
            work = tuples.map(t => Task.Visit(t.getValueNode)).toList ::: Task.ObjectDone(keys) :: work
          case _ => errors += "unsupported YAML node"; done = JNull :: done
        case Task.ArrayDone(size) =>
          val children = done.take(size).reverse.toVector; done = JArr(children) :: done.drop(size)
        case Task.ObjectDone(keys) =>
          val children = done.take(keys.size).reverse.toVector; done = JObj(keys.zip(children)) :: done.drop(keys.size)
    val found = errors.result()
    if found.nonEmpty then Left(found) else Right(done.head)

  /** Block YAML uses quoted JSON scalars; explicit worklist, no recursive emitter. */
  def writeYaml(value: Json): String =
    val out = new StringBuilder
    var work = List((value,0,""))
    while work.nonEmpty do
      val (node,indent,prefix) = work.head; work = work.tail
      val children: Vector[(Json,String)] = node match
        case JObj(fs) if fs.nonEmpty => fs.map((k,v) => v -> (Json.print(JStr(k)) + ":"))
        case JArr(vs) if vs.nonEmpty => vs.map(_ -> "-")
        case _ => Vector.empty
      if children.isEmpty then
        val _ = out.append(" " * indent).append(prefix)
        if prefix.nonEmpty then out.append(' ')
        out.append(Json.print(node)).append('\n'): Unit
      else
        if prefix.nonEmpty then out.append(" " * indent).append(prefix).append('\n'): Unit
        val nextIndent = if prefix.isEmpty then indent else indent + 2
        work = children.map((v,p) => (v,nextIndent,p)).toList ::: work
    out.toString
