package okay



/**
 * Optics fused in the COMPILER (specs/optics.md, optics-fuse).
 *
 * `optics-fast` measured why compiling an optic at run time is slower
 * than not compiling it: a live chain inlines into the call site and
 * escape analysis flattens its intermediates, while a pair stored in
 * a field hides behind a lambda the JIT will not inline through, so
 * the same intermediates escape and allocate (176 B/op became 296).
 * The answer is not to store the pair but to do the fusion where
 * inlining is not a hope — in the compiler.
 *
 * `Fuse.set(optic)(b)(s)` reads the optic EXPRESSION rather than a
 * value: a chain of `Lens(get, put)` and `Prism.some` joined by
 * `andThen`, which is exactly what `Lens[S](_.f)` expands to. It emits
 * the nested update the way a person would write it, beta-reducing
 * every lambda, so no optic and no intermediate survives.
 *
 * WHAT IT READS: an optic written literally here, or named by an
 * `inline def` (whose definition it follows), with its halves written
 * out — and, since optics-zero-tax, `Lens[S](_.f)` as well.
 *
 * That last one was recorded here as impossible, and the record was
 * half right. A macro cannot make another macro expand; it does not
 * have to. `FocusMacros.impl` is an ordinary compile-time function over
 * trees, so `plan` calls it with the selector and the Mirror it finds
 * in the call and reads the result. The correction matters because
 * `Lens[S](_.f)` is the idiomatic way to build a lens here, so while
 * it was unreadable the fusion was off for most code that wanted it.
 *
 * IT ALWAYS COMPILES. Anything it cannot read — an optic behind a
 * `val`, a traversal, a block with statements in it — falls back to
 * `optic.set(b)(s)`, the ordinary road. Correctness never depends on
 * the fusion; only speed does, and TestFuse tells the two apart at
 * run time with a poisoned interpretation rather than trusting a
 * comment.
 *
 * The macro READS the optic and WRITES the update, which the policy in
 * specs/codecs.md did not allow — see specs/optics.md, optics-fuse,
 * for the amendment and its reason.
 */
object Fuse {
  // the implementations are okay.macros.FuseMacros (okay-macros-package), private to okay and public in
  // the binary: an inline def splicing a private one otherwise gets an unstable accessor (E192)

  /** the optic's `set`, fused where the shape allows */
  transparent inline def set[C[_[_, _]], S, T, A, B](inline o: Optic[C, S, T, A, B])(inline b: B)(inline s: S)
                                                    (using fn: C[Function1]): T =
    ${ okay.macros.FuseMacros.setImpl('o, 'b, 's, 'fn) }

  /** the optic's `modify`, fused where the shape allows */
  transparent inline def modify[C[_[_, _]], S, T, A, B](inline o: Optic[C, S, T, A, B])(inline f: A => B)(inline s: S)
                                                       (using fn: C[Function1]): T =
    ${ okay.macros.FuseMacros.modifyImpl('o, 'f, 's, 'fn) }
}
