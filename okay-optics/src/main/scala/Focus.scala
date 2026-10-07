package okay


import scala.deriving.Mirror

/**
 * `Lens[S](_.field)`: the field selector as CODE, not a string
 * (specs/optics.md). A macro that reads ONLY the selector's tree —
 * the policy of specs/codecs.md restated: a macro may read what the
 * compiler already wrote, never write. The lambda itself is the
 * getter, so there is no cast anywhere and the focus type is the type
 * checker's, not a match type's; the setter is built from the Mirror.
 * Anything that is not `_.f` is refused at compile time with a
 * message; a field that does not exist is refused by the type checker
 * before this macro ever runs, with its own "did you mean".
 */
final class Focus[S <: Product]:
  inline def apply[A](inline get: S => A)(using m: Mirror.ProductOf[S]): Lens[S, S, A, A] =
    ${ okay.macros.FocusMacros.impl[S, A]('get, 'm) }
