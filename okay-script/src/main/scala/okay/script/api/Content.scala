package okay.script.api

import okay.codec.{Json, Schema}

/**
 * CONTENT: the words a site shows, as data a person edits
 * (specs/site-framework.md stage 2).
 *
 * A storefront's text is not code and not a database row. It is a
 * handful of values the owner changes without a deploy — the service
 * names, their prices, the tagline — and the sites this arc is
 * measured against keep them in FILES in the site's own tree, edited
 * through a page and committed like anything else. That shape is
 * right and is what this is: a typed read with a BAKED DEFAULT under
 * it, and a typed write.
 *
 * Three layers, in the order they are consulted:
 *
 *   1. the file, when the owner has edited one;
 *   2. the default the page bakes in, which is what ships;
 *   3. nothing — `read` cannot fail, because a site with an unreadable
 *      content file must still answer.
 *
 * A damaged file is layer 2, not a 500: an editor that writes half a
 * value must not take the site down with it. `problem` says whether
 * that happened, for a page that wants to show it.
 *
 * The path is relative to the SITE ROOT and may not leave it — the
 * same rule the router has, for the same reason.
 */
object Content:

  /** where content lives: the site root, set by the container. Outside
   * a `Site` there is none, and every read is its default. */
  private[script] val scoped: Scoped[Option[java.nio.file.Path]] = Scoped(None)

  def root: Option[java.nio.file.Path] = scoped.current

  private val problems: ThreadLocal[Vector[String]] = ThreadLocal.withInitial(() => Vector.empty)

  /** what went wrong reading content during this request, if anything
   * — a page can show it; the site does not stop for it */
  def problem: Vector[String] = problems.get()

  private[script] def clearProblems(): Unit = problems.set(Vector.empty)

  /** the file under the site root, refused if it would leave it */
  private def resolve(path: String): Option[java.nio.file.Path] =
    root.flatMap { r =>
      val p = r.resolve(path.stripPrefix("/")).normalize
      Option.when(p.startsWith(r.normalize))(p)
    }

  def exists(path: String): Boolean =
    resolve(path).exists(java.nio.file.Files.isRegularFile(_))

  /**
   * The content at `path`, or `default`.
   *
   * `default` is by-name and is NOT evaluated when the file answers —
   * a baked default is often a Vector of everything a site ships
   * with, and building it per request to throw it away is the kind of
   * waste that only shows under load.
   */
  def read[A](path: String, default: => A)(using Schema[A]): A =
    resolve(path).filter(java.nio.file.Files.isRegularFile(_)) match
      case None => default
      case Some(p) =>
        val text =
          try java.nio.file.Files.readString(p)
          catch case _: Exception => ""
        if text.isEmpty then default
        else
          okay.codec.Codecs.json(summon[Schema[A]]).decode(Json.parse(text)) match
            case Right(a) => a
            case Left(why) =>
              problems.set(problems.get() :+ s"$path: $why")
              default

  /**
   * Writes the content at `path`, creating the directories under it.
   *
   * The write is ATOMIC where the filesystem allows it — a temporary
   * file beside the target, then a move — because the reader is the
   * same site serving requests while the editor saves, and half a
   * file read by a visitor is exactly the failure the default was
   * meant to catch rather than a state to create on purpose.
   */
  def write[A](path: String, a: A)(using Schema[A]): Boolean =
    resolve(path) match
      case None => false
      case Some(p) =>
        try
          Option(p.getParent).foreach(java.nio.file.Files.createDirectories(_))
          val text = Json.write(a)
          val tmp = p.resolveSibling(p.getFileName.toString + ".tmp")
          java.nio.file.Files.writeString(tmp, text): Unit
          try
            java.nio.file.Files.move(tmp, p,
              java.nio.file.StandardCopyOption.REPLACE_EXISTING,
              java.nio.file.StandardCopyOption.ATOMIC_MOVE): Unit
          catch
            case _: java.nio.file.AtomicMoveNotSupportedException =>
              java.nio.file.Files.move(tmp, p,
                java.nio.file.StandardCopyOption.REPLACE_EXISTING): Unit
          true
        catch case _: Exception => false

  /** drops the file, so the baked default answers again — "reset to
   * what shipped", which an editor needs and a delete key is */
  def clear(path: String): Boolean =
    resolve(path).exists(p => try java.nio.file.Files.deleteIfExists(p) catch case _: Exception => false)
