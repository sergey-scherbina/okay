package okay

import scala.reflect.ClassTag

/**
 * A value with its type packed alongside it — the existential
 * package, with the evidence needed to open it again.
 *
 * What it BUYS is that opening becomes CHECKED. A bare
 * `asInstanceOf[X]` on an erased value is a promise the compiler
 * cannot read and the runtime cannot test; `as[X]` here tests the
 * class and answers `Option`, so a wrong assumption is a `None` at
 * the place it was made rather than a `ClassCastException` further
 * down, or worse, silence.
 *
 * What it does NOT buy is the elimination of the cast where the type
 * is known only by an invariant. `Chunks[A]` is `Chunk[A] ! Produce`
 * and `Produce` is the identity signature, so pattern-matching the
 * Free tree gives back a chunk whose type the match has forgotten —
 * we know it is `Chunk[A]` because the whole program says so, and no
 * `ClassTag` recovers a relation between an abstract `T` and an
 * abstract `A`. `Tagged` would make that unwrap checked at the price
 * of a wrapper per chunk on the hottest path, and the check could
 * only confirm "an ArraySeq", not "an ArraySeq of A" — erasure again.
 *
 * So it is the right tool where a value is STORED heterogeneously and
 * recovered later at a type the storer knew and the reader must
 * guess. It is the wrong tool where the type is fixed by a
 * surrounding invariant, and this library has both kinds.
 */
trait Tagged:
  type T
  def value: T
  def tag: ClassTag[T]

  /** the value at a type you name, IF that is what it is */
  def as[X](using ct: ClassTag[X]): Option[X] =
    if ct.runtimeClass == tag.runtimeClass then Some(value.asInstanceOf[X]) else None

  /** what it actually is, for a message a reader can act on */
  def typeName: String = tag.runtimeClass.getName

object Tagged:
  def apply[A](v: A)(using ct: ClassTag[A]): Tagged = new Tagged:
    type T = A
    val value: T = v
    val tag: ClassTag[T] = ct

  /** open it against a type, and say what was there if it is not */
  def expect[X](t: Tagged)(using ct: ClassTag[X]): Either[String, X] =
    t.as[X].toRight(s"expected ${ct.runtimeClass.getName}, got ${t.typeName}")
