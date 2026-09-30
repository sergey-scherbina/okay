package okay.dlm

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/** one table on the shelf: which artifact, its hash, the encoder that
 * made it, when it was put there, and what it weighs */
final case class Kept(artifact: String, hash: String, encoder: String, at: Long, size: Long,
                      /** what the table was built from (`Exemplars.origin`); "" for one that did not say */
                      origin: String = "")

/**
 * THE TABLES A MODEL HAS SERVED, KEPT BY HASH (specs/dlm-learning.md §9).
 *
 * A rebuild does not replace a table: it puts the new one beside the
 * old, both named by the hash of the table AS SERVED
 * (`Exemplars.stored`), so the hash an explanation names, the hash in
 * `Ledger.Entry.Rebuilt` and the name of the file on the shelf are one
 * string. Nothing leaves the shelf but by a `Retention`, through
 * `Shelf.prune`, and each departure is a ledger entry.
 *
 * Ours are two: a directory of checkpoints, and memory for a suite.
 * A service with an object store supplies its own.
 */
trait Shelf:
  /** keep a table; putting the same table twice keeps one */
  def put(artifact: String, table: Exemplars, at: Long): Kept
  /** the table with this hash, refused by encoder like any checkpoint,
   * and refused when its content no longer hashes to its name */
  def get(artifact: String, hash: String, expect: Option[(String, Int)] = None): Either[String, Exemplars]
  /** what is kept of one artifact, oldest first */
  def kept(artifact: String): Vector[Kept]
  def artifacts: Vector[String]
  /** true when there was such a table and now there is not */
  def drop(artifact: String, hash: String): Boolean

  def has(artifact: String, hash: String): Boolean = kept(artifact).exists(_.hash == hash)
  def all: Vector[Kept] = artifacts.flatMap(kept)

object Shelf:

  private val nameShape = "[A-Za-z0-9_-]+".r
  private val hashShape = "[0-9a-f]{32}".r
  /** a name and a hash become a file name, so neither may carry a path */
  private def check(artifact: String, hash: String): Either[String, Unit] =
    if !nameShape.matches(artifact) then Left(s"«$artifact» is not an artifact name")
    else if !hashShape.matches(hash) then Left(s"«$hash» is not a table hash")
    else Right(())

  private def verified(artifact: String, hash: String, e: Exemplars): Either[String, Exemplars] =
    if e.hash == hash then Right(e)
    else Left(s"$artifact.$hash hashes to ${e.hash} — the shelf's copy changed, refused")

  private def sizeOf(t: Exemplars, f16: Boolean): Long =
    Checkpoint.bytes(t.encoder, t.dim, t.rows.map(_.label), t.rows.map(_.vec.toArray), f16 = f16).remaining().toLong

  /** OURS IN MEMORY: for a suite, and for a process that keeps its
   * tables no longer than itself */
  def memory(f16: Boolean = true): Shelf = new Shelf:
    private var held = Map.empty[(String, String), (Exemplars, Kept)]
    def put(artifact: String, table: Exemplars, at: Long): Kept = synchronized {
      val t = table.stored(f16)
      check(artifact, t.hash).fold(e => throw IllegalArgumentException(e), identity)
      held.get((artifact, t.hash)) match
        case Some((_, k)) => k
        case None =>
          val k = Kept(artifact, t.hash, t.encoder, at, sizeOf(t, f16), t.origin)
          held += (artifact, t.hash) -> (t, k); k
    }
    def get(artifact: String, hash: String, expect: Option[(String, Int)]): Either[String, Exemplars] =
      synchronized(held.get((artifact, hash))) match
        case None => Left(s"$artifact.$hash: not on the shelf")
        case Some((t, _)) => expect match
          case Some((want, _)) if t.encoder != want => Left(s"$artifact.$hash: made by «${t.encoder}», this process runs «$want» — refused")
          case Some((_, dim)) if t.dim != dim => Left(s"$artifact.$hash: dim ${t.dim}, this process has $dim — refused")
          case _ => verified(artifact, hash, t)
    def kept(artifact: String): Vector[Kept] =
      synchronized(held.values.map(_._2).filter(_.artifact == artifact).toVector).sortBy(k => (k.at, k.hash))
    def artifacts: Vector[String] = synchronized(held.keys.map(_._1).toVector.distinct.sorted)
    def drop(artifact: String, hash: String): Boolean = synchronized {
      val was = held.contains((artifact, hash)); held -= ((artifact, hash)); was
    }

  /**
   * OURS ON DISK: `<root>/<artifact>.<hash>.safetensors`, a checkpoint
   * like any other with two more metadata fields — the artifact and
   * when it was shelved, because a file's own time is whatever the last
   * checkout said.
   */
  def directory(root: Path, f16: Boolean = true): Shelf = new Shelf:
    private def file(artifact: String, hash: String) = root.resolve(s"$artifact.$hash.safetensors")
    private def keptOf(p: Path): Option[Kept] =
      val name = p.getFileName.toString.stripSuffix(".safetensors")
      val dot = name.lastIndexOf('.')
      if dot <= 0 then None
      else
        val (artifact, hash) = (name.take(dot), name.drop(dot + 1))
        check(artifact, hash).toOption.flatMap(_ => Checkpoint.meta(p).toOption).map(m =>
          Kept(artifact, hash, m.getOrElse("encoder", ""), m.get("shelved").flatMap(_.toLongOption).getOrElse(0L),
            Files.size(p), m.getOrElse("origin", "")))
    private def files: Vector[Path] =
      if !Files.isDirectory(root) then Vector.empty
      else
        val s = Files.list(root)
        try s.iterator().asScala.filter(_.getFileName.toString.endsWith(".safetensors")).toVector
        finally s.close()

    def put(artifact: String, table: Exemplars, at: Long): Kept = synchronized {
      val t = table.stored(f16)
      check(artifact, t.hash).fold(e => throw IllegalArgumentException(e), identity)
      val p = file(artifact, t.hash)
      if !Files.exists(p) then
        Checkpoint.write(p, t.encoder, t.dim, t.rows.map(_.label), t.rows.map(_.vec.toArray),
          Exemplars.provenanceMeta(t) ++ Map("artifact" -> artifact, "shelved" -> at.toString), f16 = f16)
      keptOf(p).getOrElse(throw IllegalStateException(s"$p: written and not readable"))
    }
    def get(artifact: String, hash: String, expect: Option[(String, Int)]): Either[String, Exemplars] =
      for
        _ <- check(artifact, hash)
        l <- Checkpoint.read(file(artifact, hash), expect).left.map(w =>
          if Checkpoint.absent(w) then s"$artifact.$hash: not on the shelf" else w)
        e <- verified(artifact, hash, Exemplars.ofCheckpoint(l))
      yield e
    def kept(artifact: String): Vector[Kept] =
      files.flatMap(keptOf).filter(_.artifact == artifact).sortBy(k => (k.at, k.hash))
    def artifacts: Vector[String] = files.flatMap(keptOf).map(_.artifact).distinct.sorted
    def drop(artifact: String, hash: String): Boolean = synchronized {
      check(artifact, hash).isRight && Files.deleteIfExists(file(artifact, hash))
    }

  /** the same layout read from a jar: a service's image carries its
   * shelf, and a pin by hash is served from it without a rebuild */
  def resource(prefix: String, artifact: String, hash: String,
               expect: Option[(String, Int)] = None): Either[String, Exemplars] =
    for
      _ <- check(artifact, hash)
      l <- Checkpoint.resource(s"${prefix.stripSuffix("/")}/$artifact.$hash.safetensors", expect).left.map(w =>
        if Checkpoint.absent(w) then s"$artifact.$hash: not on the shelf in the image" else w)
      e <- verified(artifact, hash, Exemplars.ofCheckpoint(l))
    yield e

  /**
   * WHAT A POLICY LETS GO — and never the newest table of an artifact,
   * nor one named in `serving` as `(artifact, hash)`, whatever the
   * policy says. Each table dropped is a `Pruned` entry naming the
   * policy, returned for the caller's ledger.
   */
  def prune(shelf: Shelf, keep: Retention, serving: Set[(String, String)], now: Long, by: String)
  : Vector[Ledger.Entry] =
    shelf.artifacts.flatMap { a =>
      val kept = shelf.kept(a)
      val newest = kept.lastOption.map(_.hash)
      keep.expired(kept, now)
        .filterNot(k => newest.contains(k.hash) || serving((a, k.hash)))
        .filter(k => shelf.drop(a, k.hash))
        .map(k => Ledger.Entry.Pruned(a, k.hash, keep.name, now, by))
    }

/**
 * WHICH OLD TABLES MAY GO (specs/dlm-learning.md §9). A policy sees
 * one artifact's kept tables, oldest first, and names those it would
 * let go; `Shelf.prune` then spares the newest and the serving ones
 * whatever it said. OURS KEEPS EVERYTHING: a table leaves the shelf
 * because somebody chose a policy, never by default.
 */
trait Retention:
  /** what a `Pruned` entry says dropped the table */
  def name: String
  def expired(kept: Vector[Kept], now: Long): Vector[Kept]

object Retention:

  val all: Retention = new Retention:
    val name = "all"
    def expired(kept: Vector[Kept], now: Long) = Vector.empty

  given ours: Retention = all

  /** the newest `n` stay */
  def latest(n: Int): Retention =
    require(n >= 1, "a shelf keeps at least the table it serves")
    new Retention:
      val name = s"last:$n"
      def expired(kept: Vector[Kept], now: Long) = kept.sortBy(_.at).dropRight(n)

  /** what was shelved less than `ms` ago stays */
  def within(ms: Long, label: String = ""): Retention = new Retention:
    val name = if label.nonEmpty then label else s"within:${ms}ms"
    def expired(kept: Vector[Kept], now: Long) = kept.filter(k => now - k.at >= ms)

  /** a table that ANY of these keeps, stays */
  def any(keep: Retention*): Retention = new Retention:
    val name = keep.map(_.name).mkString(",")
    def expired(kept: Vector[Kept], now: Long) =
      val gone = keep.map(_.expired(kept, now).map(_.hash).toSet)
      kept.filter(k => gone.nonEmpty && gone.forall(_(k.hash)))

  private val day = 24L * 60 * 60 * 1000

  /** a policy by its name: `all`, `last:N`, `days:N`, or several with
   * commas, kept if any keeps — the spelling a config value uses */
  def of(spec: String): Either[String, Retention] =
    val parts = spec.split(",").map(_.trim).filter(_.nonEmpty).toVector
    val each = parts.map {
      case "all" => Right(all)
      case s if s.startsWith("last:") => s.drop(5).toIntOption.filter(_ >= 1).toRight(s"«$s»: last:N with N ≥ 1").map(latest)
      case s if s.startsWith("days:") => s.drop(5).toIntOption.filter(_ >= 0).toRight(s"«$s»: days:N with N ≥ 0").map(d => within(d * day, s))
      case s => Left(s"«$s» is not a retention policy (all, last:N, days:N)")
    }
    each.collectFirst { case Left(e) => e } match
      case Some(e) => Left(e)
      case None =>
        val ps = each.collect { case Right(p) => p }
        if ps.isEmpty then Left("an empty retention policy")
        else if ps.lengthIs == 1 then Right(ps.head)
        else Right(any(ps*))
