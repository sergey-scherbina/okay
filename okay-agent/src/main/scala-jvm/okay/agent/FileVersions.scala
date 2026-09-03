package okay.agent

import okay.codec.Json

import java.nio.file.{Files, Path, Paths, StandardCopyOption}
import scala.jdk.CollectionConverters.*

/**
 * A [[Rerun.Versions]] that outlives the process: one JSON file per
 * version in a directory, named by the version's id.
 *
 * The on-disk shape is its OWN model, written and read here rather
 * than derived from the in-memory types. That is the same call
 * `Staged`/`Json.mergePatch` make elsewhere in this codebase and that
 * rozum's `replay.rs` makes in Rust: a storage format is a decision
 * with its own compatibility story, and deriving it from whatever the
 * runtime types happen to be turns every internal rename into a
 * silent format change. Here the mapping is thirty lines and the file
 * is legible to a person with `cat`, which is most of the point — a
 * version tree nobody can read by hand is a tree nobody audits.
 *
 * Layout, deliberately flat:
 * {{{
 *   <dir>/<version-id>.json     one version, whole
 * }}}
 * The tree is not in the layout because it does not need to be: every
 * version names its `parent`, so `Versions.lineage` walks it, and a
 * directory listing is the set of all versions. A nested layout would
 * have to be rewritten whenever a branch appears, which is exactly
 * when nothing should have to move.
 *
 * Writes are atomic (a temp file, then a rename), so a reader never
 * sees half a version — a version is only ever whole or absent, which
 * is the same promise the journal itself makes about an entry.
 */
final class FileVersions(dir: Path) extends Rerun.Versions {

  Files.createDirectories(dir): Unit

  private def fileOf(id: String): Path = dir.resolve(s"$id.json")

  def put(v: Rerun.Version): Unit =
    val text = Json.print(FileVersions.encode(v))
    val tmp = dir.resolve(s".${v.id}.tmp")
    Files.writeString(tmp, text): Unit
    Files.move(tmp, fileOf(v.id), StandardCopyOption.REPLACE_EXISTING): Unit

  def get(id: String): Option[Rerun.Version] =
    val f = fileOf(id)
    if !Files.exists(f) then None
    else FileVersions.decode(Json.parseValue(Files.readString(f)))

  /** every version in the directory, oldest file first — a listing,
   * not a history: the ORDER of a branch is in the parent pointers,
   * and mtime is only how the directory happens to be sorted */
  def all: Vector[Rerun.Version] =
    val files = Files.list(dir).iterator().asScala.toVector
      .filter(p => p.getFileName.toString.endsWith(".json"))
      .sortBy(p => (Files.getLastModifiedTime(p).toMillis, p.getFileName.toString))
    files.flatMap(p => FileVersions.decode(Json.parseValue(Files.readString(p))))
}

object FileVersions {

  def at(path: String): FileVersions = new FileVersions(Paths.get(path))

  // ── the on-disk model ───────────────────────────────────────────

  private def str(s: String): Json = Json.JStr(s)
  private def num(i: Int): Json = Json.JNum(i.toDouble)
  private def obj(fs: (String, Json)*): Json = Json.JObj(fs.toVector)

  private def encodeEntry(e: Durable.Entry): Json =
    obj("seq" -> num(e.seq), "op" -> str(e.op), "fingerprint" -> str(e.fingerprint),
        "key" -> str(e.key),
        "answer" -> e.answer.fold(Json.JNull)(str))

  private def encodeProvenance(p: Rerun.Provenance): Json =
    obj("revision" -> str(p.revision), "model" -> str(p.model),
        "tools" -> str(p.tools), "note" -> str(p.note))

  private def encodeDivergence(d: Rerun.Divergence): Json =
    obj("seq" -> num(d.seq), "call" -> str(d.call),
        "kind" -> str(d.kind match
          case Rerun.Divergence.Kind.Answer => "answer"
          case Rerun.Divergence.Kind.Call => "call"),
        "recorded" -> str(d.recorded), "got" -> str(d.got))

  def encode(v: Rerun.Version): Json =
    obj("id" -> str(v.id),
        "parent" -> v.parent.fold(Json.JNull)(str),
        "branchedAt" -> v.branchedAt.fold(Json.JNull)(num),
        "provenance" -> encodeProvenance(v.provenance),
        "divergence" -> v.divergence.fold(Json.JNull)(encodeDivergence),
        "entries" -> Json.JArr(v.entries.map(encodeEntry)))

  // ── reading it back ─────────────────────────────────────────────

  private def field(j: Json, name: String): Option[Json] = j match
    case Json.JObj(fs) => fs.find(_._1 == name).map(_._2)
    case _ => None

  private def asStr(j: Option[Json]): Option[String] = j.collect { case Json.JStr(s) => s }
  private def asInt(j: Option[Json]): Option[Int] = j.collect { case Json.JNum(n) => n.toInt }

  private def decodeEntry(j: Json): Option[Durable.Entry] =
    for
      seq <- asInt(field(j, "seq"))
      op <- asStr(field(j, "op"))
      fp <- asStr(field(j, "fingerprint"))
      key <- asStr(field(j, "key"))
    yield Durable.Entry(seq, op, fp, key, asStr(field(j, "answer")))

  private def decodeProvenance(j: Option[Json]): Rerun.Provenance =
    j.fold(Rerun.Provenance())(p => Rerun.Provenance(
      asStr(field(p, "revision")).getOrElse(""),
      asStr(field(p, "model")).getOrElse(""),
      asStr(field(p, "tools")).getOrElse(""),
      asStr(field(p, "note")).getOrElse("")))

  private def decodeDivergence(j: Json): Option[Rerun.Divergence] =
    for
      seq <- asInt(field(j, "seq"))
      call <- asStr(field(j, "call"))
      kind <- asStr(field(j, "kind")).map {
        case "call" => Rerun.Divergence.Kind.Call
        case _ => Rerun.Divergence.Kind.Answer
      }
      recorded <- asStr(field(j, "recorded"))
      got <- asStr(field(j, "got"))
    yield Rerun.Divergence(seq, call, kind, recorded, got)

  /** A file that does not parse as a version is None rather than an
   * exception: a directory is a place other things end up, and one
   * unreadable file must not make the rest of the tree unreadable. */
  def decode(j: Json): Option[Rerun.Version] =
    for
      id <- asStr(field(j, "id"))
      entries <- field(j, "entries") match
        case Some(Json.JArr(vs)) =>
          val decoded = vs.flatMap(decodeEntry)
          if decoded.length == vs.length then Some(decoded) else None
        case _ => None
    yield Rerun.Version(
      id = id,
      entries = entries,
      provenance = decodeProvenance(field(j, "provenance")),
      parent = asStr(field(j, "parent")),
      branchedAt = asInt(field(j, "branchedAt")),
      divergence = field(j, "divergence").flatMap(decodeDivergence))
}
