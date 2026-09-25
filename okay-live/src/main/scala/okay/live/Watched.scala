package okay.live

import okay.*
import okay.given
import okay.codec.{Json, JsonOptic, Schema}
import scala.annotation.tailrec

/**
 * SUBSCRIBE TO A LENS (specs/optics-outside.md, stage 10): a document
 * many viewers watch through paths. A subscription names a dotted
 * key — `customer.address`, `lines[1].qty` — which is the WIRE FORM
 * of a lens: it survives serialisation, a client can send it, and
 * `JsonOptic.path` compiles it against the document's `Schema` into
 * the affine that reads and writes there. On every change the
 * document pushes each subscriber ONLY its focused part, and only
 * when that part changed; a client write is `set(key, value)`, the
 * same affine's `set`. A `TypedZipper`'s `pathKey` is such a key, so
 * a position the code chose by type is a subscription.
 *
 * Built on what was here: the document in a `TRef` (the core's
 * single cell), the subscribers in a `TList` as `Hub` keeps them, a
 * `Channel` per subscriber. The same honest limit as `Hub`: a closed
 * subscriber's channel stays remembered until process end.
 *
 * A subscriber whose focus becomes ABSENT (the field removed) is told
 * `JNull` once: absence is a change it must see, and `null` is the
 * document's own spelling of it.
 */
final class Watched[A](initial: Json)(using s: Schema[A]):
  private val cell = TRef[Json](initial)
  private final case class Sub(key: String, at: Affine[Json, Json, Json, Json], out: Channel[Json])
  private val subs = TList.empty[Sub]
  private val root: Affine[Json, Json, Json, Json] = Affine(Right(_), (_, v) => v)

  /** the whole document, now */
  def get: Json = cell.get

  /** the lens a key names — the root for "", refused BY NAME otherwise */
  def lens(key: String): Either[String, Affine[Json, Json, Json, Json]] =
    if key.isEmpty then Right(root)
    else JsonOptic.path(s, key).toRight(s"`$key` names nothing the schema of ${Watched.nameOf(s)} writes")

  /** the focused part, now */
  def focus(key: String): Either[String, Option[Json]] = lens(key).map(_.preview(get))

  /** a channel that receives the focused part each time it changes */
  def subscribe(key: String): Either[String, Channel[Json]] = lens(key).map { at =>
    val c = Channel[Json]()
    subs.append(Sub(key, at, c))
    c
  }

  /** the whole document rewritten; every subscriber whose part changed is told */
  def modify(f: Json => Json): Unit =
    val (before, after) = cell.modify(old => { val n = f(old); (n, (old, n)) })
    if before != after then
      subs.snapshot.foreach { sub =>
        val was = sub.at.preview(before)
        val now = sub.at.preview(after)
        if was != now then sub.out.offer(now.getOrElse(Json.JNull)): Unit
      }

  /** a client write: the part at `key` replaced, through the same lens a subscriber reads by */
  def set(key: String, value: Json): Either[String, Unit] = lens(key).map(at => modify(at.set(value)))

  /** the typed door: the whole document replaced by a value, through the codec */
  def put(a: A): Unit = modify(_ => Json.parse(Json.write(a)))

object Watched:
  @tailrec private def nameOf(s: Schema[?]): String = s match
    case p: Schema.SProduct[?] => p.name
    case su: Schema.SSum[?] => su.name
    case Schema.SIso(u, _, _) => nameOf(u())
    case other => other.toString
