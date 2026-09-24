package okay2

/**
 * A ROW is a requirement: the signatures a program may perform. A
 * SIGNATURE is a Row whose `Op` names its operations; a row of several
 * is their INTERSECTION, `State[Int] + Writer[String]`, where `+` is
 * `with` (package.scala) — and `Free` is CONTRAVARIANT in it, so a
 * program needing less is already a program in any row that needs more
 * (specs/okay2.md, stage 8). `Row` itself is the empty requirement,
 * `Pure`: the top of the order, which every row is below.
 *
 * `Op` is read ONLY at a single signature. On an intersection scalac 2
 * resolves `#Op` to the LAST parent's (`(Writer with State)#Op =:=
 * State.Op`, measured), and reading an operation at that type inserts a
 * checkcast that fails on the other signature's operations. So `Inject`
 * holds an operation as `Any`, and the typed view `F#Op[A]` is handed
 * out by `Split` after F's class test — the one cast the row ever made.
 *
 * WHY NOT A HIGHER-KINDED ALIAS (measured, stage 0): `type +[F[_],
 * G[_]] = Or[F, G]#Row` is refused by scalac 2.13.18 ("type Row takes
 * type parameters"). A Row of kind `*` needs none of that.
 */
trait Row { type Op[+A] }
