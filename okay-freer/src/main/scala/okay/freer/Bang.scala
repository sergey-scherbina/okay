package okay.freer

import okay.{Answers, Control}
import okay.given

/** the union of two signatures: F + G — the classic's row (the core's `+` is the machine's, on a nominal row) */
infix type +[F[+_], G[+_]] = [A] =>> F[A] | G[A]

/** the empty signature: a computation over it is pure, with nothing to perform; the zero of `+` */
type Pure[+A] = Nothing

/** fix the parameter of a binary signature: State % S, Throws % E */
infix type %[F[_, _], S] = F[S, *]

/**
 * A partial function, infix: `Request |=> Response ! Async`. The spelling is forced by `!`: an infix type's
 * precedence comes from its first character, and anything binding tighter than `!` (`~>`, `-?>`, `=?>`)
 * parses `A ~> B ! F` as `(A ~> B) ! F`. A union on the left binds first, so `Get | Post |=> Res` reads as
 * it looks.
 */
infix type |=>[A, B] = PartialFunction[A, B]

/** a computation of A performing the operations of F: A ! F */
infix type ![A, F[+_]] = Free[F, A]

/** the toolkit's short name, as in `!.run`: the classic's companion */
val ! = Classic

/** a value as a computation */
inline def pure[F[+_], A](a: A): A ! F = Free.pure(a)

/** an operation as a computation */
inline def effect[F[+_], A](a: F[A]): A ! F = Free.inject(a)

/**
 * An operation performed, postfix: `Users.Find(7).perform : Option[String] ! Users`. The answer type comes
 * from the case (`Find` extends `Users[Option[String]]`), so nothing is written twice. Named constructors
 * are still the better API for an effect others will use. It applies to any `F[A]`: `List(1, 2).perform` is
 * nondeterminism, which `runSeq` (Choice.scala) handles.
 */
extension [F[+_], A](op: F[A])
  inline def perform: A ! F = effect(op)

/** THE DEFAULT: the tree with THE MACHINE (okay-cont) as its carrier — a handler `F !> S` is a program of the
 * machine. The CPS `Cps` is one import away, `import okay.freer.cps.*` (stage 33) */
given given_Classic_Free: FreeEffectsAt[okay.cont.Carrier] = FreeEffects
val FreeEffects: FreeEffectsAt[okay.cont.Carrier] = FreeEffectsAt(summon[Control[okay.cont.Carrier]])

/** the tree at the CPS carrier, `Cps`, as it was: `import okay.freer.cps.{given_Classic_Free, *}` chooses it, with its `!>` and `handler` */
object cps:
  /** the SAME NAME as the default's: imported BY NAME it shadows the package's, so the search sees one instance (a `given` wildcard imports by type and shadows nothing) */
  given given_Classic_Free: FreeEffectsAt[Cps] = FreeEffectsAt(summon[Control[Cps]])
  infix type !>[F[_], S] = Interpr[F, Cps, S]
  inline def handler[F[_] : Answers as H, S]: F !> S = [X] => e => Cps.Pure(H.handle(e))
