package okay

import okay.cont.Handler
import scala.annotation.implicitNotFound
import scala.quoted.*

/**
 * Extensible effects: THE INTERFACE, over ROWS, AND THE FACADE (specs/freer-min.md, stages 47–48). `Effects[M]` is
 * what every encoding of a program implements — the machine's `Free[R, A]` (okay-cont) and the classic tree
 * under it (`okay.freer.Rowed`) — and what code generic in the encoding is written over. A row is a nominal
 * list of effects, `Ask +: Say +: Pure`, written either way (`Pure + Ask + Say`); an operation is performed by
 * its PATH in the row (`Member`, the compiler builds it), a handler takes its effect off the row WHEREVER it is
 * (`Removed`): the order of effects in a type says nothing, the order of handlers everything. Handlers are the
 * machine's: `Answering` answers in place (state, reader, writer), `Handler` has the continuation (choose,
 * dialogue); an encoding runs them its own way.
 *
 * THE WORDS A PROGRAM IS WRITTEN IN ARE MEMBERS OF THE INSTANCE (the operator, stage 48: the facade defined once,
 * as aliases over the typeclass, dispatched statically): `A ! R`, `effect`, `op.perform`, `p.handle(h)`,
 * `p.value`, each an alias over the primitives above, so they name the REAL types of one encoding and no other.
 * Which encoding is the IMPORT's choice: `Effects.machine` is the machine's (`okay.*` exports it, the default),
 * `okay.freer.tree` the tree's, a third encoding's is its given. The rows are one vocabulary, below.
 */

/** the rows, the machine's, named at the core's door: aliases, not an `export` — a top-level export here and the
 * facade's (`export machine.*`, Facade.scala) resolve against each other and the compiler drops both (stage 48) */
type Row = okay.cont.Row
infix type +:[E[+_], T <: Row] = okay.cont.+:[E, T]
infix type +[R <: Row, E[+_]] = okay.cont.+[R, E]
infix type %[F[_, +_], S] = okay.cont.%[F, S]
type Pure = okay.cont.Pure
type Union[R <: Row, X] = okay.cont.Union[R, X]
type Member[E[+_], R <: Row] = okay.cont.Member[E, R]
type Removed[E[+_], R <: Row] = okay.cont.Removed[E, R]
type Sub[R1 <: Row, R2 <: Row] = okay.cont.Sub[R1, R2]
type Tagged[R <: Row, +X] = okay.cont.Tagged[R, X]
val Tagged: okay.cont.Tagged.type = okay.cont.Tagged
type Members[R <: Row] = okay.cont.Members[R]
val Members: okay.cont.Members.type = okay.cont.Members

trait Effects[M[_ <: Row, _]]:
  def pure[R <: Row, A](a: A): M[R, A]
  /** an operation, by its path in the row */
  def perform[E[+_], R <: Row, X](op: E[X])(using Member[E, R]): M[R, X]
  /** a bind whose left side is deferred, forced only when the encoding's interpreter reaches it, so that
   * mutually recursive functions returning `M[R, A]` call each other in tail position with no JVM frame each */
  def defer[R <: Row, A, B](thunk: () => M[R, A])(f: A => M[R, B]): M[R, B]
  /** a tail call to a mutually recursive function, for code written over any `M: Effects` */
  def tailcall[R <: Row, A](thunk: => M[R, A]): M[R, A] = defer(() => thunk)(pure)

  extension [R <: Row, A](m: M[R, A])
    /** at the one row: a program's row is declared, its operations find their paths in it */
    def flatMap[B](f: A => M[R, B]): M[R, B]
    inline def map[B](f: A => B): M[R, B] = m.flatMap(a => pure(f(a)))

  /** the effect `E` handled, wherever it is in the row; the result over the row without it. A handler that
   * answers in place (`Answering`, `h.inPlace`) takes the road with no delimiter */
  def handle[E[+_], A, Ans, R <: Row](h: Handler[E, A, Ans])(m: M[R, A])(using rm: Removed[E, R]): M[rm.Out, Ans]
  /** a program with nothing left to handle, to its value */
  def run[A](m: M[Pure, A]): A

  // THE FACADE: the words, each an alias over the primitives above

  /** a program of `A` over the row `R`: `Int ! (State % Int +: Say +: Pure)`, `Int ! (Pure + State % Int + Say)` */
  infix type ![A, R <: Row] = M[R, A]

  /** an operation as a program, over any row that has its effect (`Op`): the row is the program's it is bound into */
  def effect[F[+_], X](op: F[X]): Op[F, X] = Op(op)

  extension [F[+_], X](op: F[X])
    /** an operation performed, postfix: `State.Get[Int]().perform` */
    def perform: Op[F, X] = Op(op)

  extension [R <: Row, A](p: M[R, A])
    /** the effect handled, wherever it is in the row: `p.handle(State(0))` */
    def handle[F[+_], Ans](h: Handler[F, A, Ans])(using rm: Removed[F, R]): M[rm.Out, Ans] = this.handle[F, A, Ans, R](h)(p)

  extension [A](p: M[Pure, A])
    /** a program with nothing left to handle, run: its value */
    def value: A = run(p)

  /**
   * AN OPERATION AS A PROGRAM OVER ANY ROW THAT HAS ITS EFFECT: the row is not the operation's to say — it is the
   * program's it is bound into, so `flatMap` and `map` take it from the expected type (`Member`, the path the
   * compiler builds), as the classic's `effect` took its signature from the context; a bare operation becomes a
   * program by the same path where one is expected (`at`, its row inferred from the expected type — no
   * conversion: one warns at every use site, op-at). A `for` over mixed effects declares
   * the program's row and nothing else.
   */
  final class Op[F[+_], X](val op: F[X]):
    def flatMap[R <: Row, B](f: X => M[R, B])(using m: Member[F, R]): M[R, B] = perform[F, R, X](op).flatMap(f)
    def map[R <: Row, B](f: X => B)(using m: Member[F, R]): M[R, B] = perform[F, R, X](op).map(f)
    /** the operation as a program at a row, written */
    def at[R <: Row](using m: Member[F, R]): M[R, X] = perform[F, R, X](op)


object Effects:
  /** THE DEFAULT: the machine's program itself — the instance found with no import, as a companion's given is,
   * and the facade `okay.*` exports (below) */
  given machine: Effects[okay.cont.Free] with
    def pure[R <: Row, A](a: A): okay.cont.Free[R, A] = okay.cont.Free.pure(a)
    def perform[E[+_], R <: Row, X](op: E[X])(using m: Member[E, R]): okay.cont.Free[R, X] = okay.cont.Free.inject(op).at[R]
    def defer[R <: Row, A, B](thunk: () => okay.cont.Free[R, A])(f: A => okay.cont.Free[R, B]): okay.cont.Free[R, B] =
      okay.cont.Free.defer(thunk)(f)
    override def tailcall[R <: Row, A](thunk: => okay.cont.Free[R, A]): okay.cont.Free[R, A] = okay.cont.Free.delay(() => thunk)
    extension [R <: Row, A](m: okay.cont.Free[R, A])
      def flatMap[B](f: A => okay.cont.Free[R, B]): okay.cont.Free[R, B] = m.flatMap(f)
    def handle[E[+_], A, Ans, R <: Row](h: Handler[E, A, Ans])(m: okay.cont.Free[R, A])(using rm: Removed[E, R]): okay.cont.Free[rm.Out, Ans] =
      h.inPlace match
        case Some(a) => okay.cont.Free.handle(a)(m)
        case None => okay.cont.Free.handle(h)(m)
    def run[A](m: okay.cont.Free[Pure, A]): A = okay.cont.Machine.value(okay.cont.Free.top(m))

  /** any encoding in direct style: `M[R, *]` as a monad, for `direct[[A] =>> M[R, A]]` over `Effects[M]` */
  def monad[M[_ <: Row, _], R <: Row](using E: Effects[M]): Monad[[A] =>> M[R, A]] = new Monad[[A] =>> M[R, A]]:
    def pure[A](a: A): M[R, A] = E.pure(a)
    extension [A](a: M[R, A])
      def flatMap[B](f: A => M[R, B]): M[R, B] = E.flatMap(a)(f)

  /** the instance in scope, by its encoding: `Effects[Free]` */
  inline def apply[M[_ <: Row, _]](using E: Effects[M]): E.type = E

/** `import okay.*` is the machine's facade: `A ! R`, `effect`, `op.perform`, `p.handle(h)`, `p.value`, `Op` */
export Effects.machine.{given, *}

// ── control: the interface of delimited continuations (was Control.scala) ─────────────────────────────────────

/**
 * THE CONTROL INTERFACE, common to every monad of delimited continuations: Danvy–Filinski's `shift` and `/`
 * (`reset` is `/ identity`) over a parameterised monad `M[A, S, R]`, `(A => S) => R`. The instances: `Cont`
 * (okay-freer: the freer tree read as continuations, data, stack-safe), `Func` (closures, the reference, not
 * stack-safe), and the machine's `Carrier` (okay-cont). The interface and every instance are the core's: the two
 * modules know nothing of each other, nor of this.
 */
trait Control[M[_, _, _]] extends ParaMonad[M]:
  def shift[A, S, R](f: (A => S) => R): M[A, S, R]
  extension [A, S, R](m: M[A, S, R])
    infix def /(k: A => S): R
  inline def reset[A, R](m: M[A, A, R]): R = m / identity
  /** is `m` an answer already — a `pure`, nothing captured? then `answerOf` has it, and a handler's loop goes on
   * with a tail call instead of a continuation (`Effects.handle`'s tail answer). Closures cannot tell: never */
  def isAnswer[A, S](m: M[A, S, S]): Boolean = false
  def answerOf[A, S](m: M[A, S, S]): A = throw IllegalStateException("not an answer: ask isAnswer first")

/** summons the instance at its precise type, so its inline operations resolve statically (Carette-Kiselyov-Shan staging) */
transparent inline def Control[M[_, _, _]]: Control[M] =
  compiletime.summonInline[Control[M]]


/** closures: the reference instance, fast, not stack-safe */
type Func[A, S, R] = (A => S) => R

given Control[Func] with
  override inline def pure[A, R](a: A): Func[A, R, R] = _(a)
  override inline def shift[A, S, R](f: (A => S) => R): Func[A, S, R] = f
  extension [A, S, R](m: Func[A, S, R])
    override inline infix def /(k: A => S): R = m(k)
    override inline def flatMap[B, S2](f: A => Func[B, S2, S]): Func[B, S2, R] =
      k => m(f(_)(k))
    // composed directly, no `pure` per element
    override inline def map[B](f: A => B): Func[B, S, R] = k => m(x => k(f(x)))

/** THE MACHINE (okay-cont) as a Control: `shift` is `Shift0` at the top level, `/` the delimiter around a `Bind`, `flatMap` a `Bind`. The `k` a body
 * is given is STRICT — a run of its own, on the host stack (as `Func`'s is; the machine's own `k` is a
 * program, `okay.cont.shift0`) */
given Control[cont.Carrier] with
  def pure[A, R](a: A): cont.Carrier[A, R, R] = cont.Cont.Return(a)
  def shift[A, S, R](f: (A => S) => R): cont.Carrier[A, S, R] =
    cont.Cont.Shift0[EmptyTuple, EmptyTuple, EmptyTuple, S, R, A](k => cont.Cont.Return(f(a => cont.Machine.value(k(a)))))
  extension [A, S, R](m: cont.Carrier[A, S, R])
    infix def /(k: A => S): R = cont.Machine.value(cont.Cont.Reset[EmptyTuple, EmptyTuple, S, R](cont.Cont.Bind(m, (a: A) => cont.Cont.Return(k(a)))))
    def flatMap[B, S2](f: A => cont.Carrier[B, S2, S]): cont.Carrier[B, S2, R] = cont.Cont.Bind(m, f)
    override def map[B](f: A => B): cont.Carrier[B, S, R] = cont.Cont.Bind(m, (a: A) => cont.Cont.Return(f(a)))
  override def isAnswer[A, S](m: cont.Carrier[A, S, S]): Boolean = m match
    case cont.Cont.Return(_) => true
    case _ => false
  override def answerOf[A, S](m: cont.Carrier[A, S, S]): A = m match
    case cont.Cont.Return(a) => a
    case _ => throw IllegalStateException("not an answer")

// ── answers: what a signature says about itself, and what performs it (was Answers.scala) ─────────────────────

/**
 * What a signature says about itself, and the tiny capability surface
 * that lets it say it to the optional direct DSL too — the runtime
 * evidence a row split needs (`TypeableK`), the marker that promotes
 * it to an effect (`Effect`), and the two-type contract
 * (`DirectEffect`/`DirectCtx`) `Effect` carries so that `okay` stays
 * usable without `okay-direct`: no macro or runtime implementation
 * lives here, only the capability `okay-direct`'s bridge exposes this
 * same evidence through. `Answers`, below, is what actually PERFORMS
 * an effect once split — the two live in one file because `Answers
 * .union` reaches for `TypeableK`/`split` directly, not just by
 * convention.
 */

/** The tiny capability surface shared by the effect kernel and the
 * optional direct DSL. It deliberately contains no macro or runtime
 * implementation, so `okay` remains usable without `okay-direct`. */
@implicitNotFound("no Direct.Effect[${F}]: auto-coloring is OPT-IN per signature.\nRegister the effect once — `given Direct.Effect[${F}] with {}` — or use the explicit marks\n(.reflect / .? / !prog), which need no marker.")
trait DirectEffect[F[_]]

/** `DirectCtx`, the evidence installed only while a `direct` block is being compiled, is in okay-freer
 * (DirectCtx.scala, package `okay`): `Cont.direct` needs it there, and the direct DSL and both monads share it */

/** ∀X, the runtime test for F[X], by the erasure of F */
@implicitNotFound("no TypeableK[${F}].\nSplitting a row needs a runtime test for ${F}'s operations, and a signature declares its own:\n  enum YourOp[+A] derives Effect\nA parameterised one says the same: `enum YourOp[S, +A] derives Effect` abstracts the LAST\nparameter, and the test is then by class only (a row may hold one of it).\nA ROW needs no instance: the split tests one side and takes the other by exclusion.")
trait TypeableK[F[_]]:
  /** is `x` an operation of F — asked by `split` on every operation
   * of every runner (split-without-either), and the WHOLE interface:
   * there used to be an `unapply` beside it answering
   * `Option[x.type & F[A]]`, and nothing in the repository ever
   * matched with it — every runner refines through `split` and then
   * matches the constructor. The extractor cost each instance a
   * method and a cast for a question `test` answers with neither
   * (core-cleanup, 2026-09-15). */
  def test(x: Any): Boolean

/**
 * A `TypeableK` by the runtime CLASS of a signature's values.
 *
 * For a signature whose ONLY parameter is the answer type — `Async`,
 * `Choose`, `Resource`, an agent's `Model` — this test is COMPLETE:
 * the answer type is erased anyway, so the class is the whole
 * identity of the operation, and there is nothing left to check.
 * Say that once, here, rather than let the compiler say "cannot be
 * checked at runtime" at every one of a hundred use sites for a test
 * that is in fact total.
 *
 * For a PARAMETERISED signature (`Writer % W`, `State % S`,
 * `Throws % E`) the class is NOT the whole identity, and this is the
 * wrong instance to reach for: see `TypeableK.byClassPartial`.
 */

def typeableK[F[_]](cls: Class[?]): TypeableK[F] = Effect.ByClass[F](cls)

/**
 * The limitation of a class test, stated once — for a signature whose
 * PARAMETER leaves no runtime trace (`Reader % R`, `State % S`,
 * `Take % V`) the test says only "this is a Reader", not "this is a
 * Reader of Int". So a row may hold ONE instance of such a signature,
 * and `Distinct[R]`, which `Row.union` requires, refuses the row
 * at COMPILE time rather than leaving it to the first wrong answer. A
 * test that is finer than the class says so in its declared type
 * (`TypeableK.ByValue`) and is allowed to repeat; `Writer.byValue.writerK`
 * is the one that does, an opt-in — Writer's DEFAULT test is the class
 * of `Say`, total and warning-free (writer-typeablek-by-class). Two — `Reader % Int + Reader % String` — misroute, and
 * `TestRowIdentity` demonstrates exactly how (the first handler
 * answers both asks and the second continuation gets a
 * ClassCastException: loud, at the first wrong answer).
 *
 * (`typeableKByClass` used to be a second name for `typeableK` that
 * carried this paragraph; nothing called it — core-cleanup.)
 */

/**
 * There is NO generic instance any more, and that is the point.
 *
 * There used to be one — `given [F[+_]](using Typeable[F[Nothing]])`,
 * an erasure test derived for any signature that had not declared
 * one. It cost more than it saved. It made every effect that forgot
 * to declare a test work anyway, at a warning per USE site ("the type
 * test for F[Nothing] cannot be checked at runtime") that the author
 * of the effect never saw. It shadowed better instances when brought
 * into lexical scope by `import okay.given`, which is why `Model`,
 * `Tool` and `Context` kept getting the erasure test after being
 * given a total one. And it was the one place in this library that
 * NEEDED the row's covariance, since `F[Nothing] <: F[X]` is what
 * made it sound (specs/writer-covariance.md, signature-covariance).
 *
 * Now a signature says `derives Effect` and its instance lives in its
 * own companion, where implicit search finds it with no import and
 * nothing can shadow it. What was lost with the fallback: a COMPOSITE
 * row can no longer be given a test implicitly. Nothing needs one —
 * `Row.union[F, G]` and `<|>` test one side and take the other by
 * exclusion, so every tested signature is atomic.
 */
object TypeableK:

  /**
   * A TEST THAT READS THE OPERATION'S VALUE, and so tells two
   * instances of one signature apart.
   *
   * The default is the opposite: a test is the erasure, and a row may
   * hold ONE member of a signature (see `typeableKByClass`). An
   * instance that does better says so HERE, in its declared type,
   * because nothing else can be read by a macro — and `Distinct[R]`
   * reads exactly this to decide whether `Writer % String + Writer %
   * Int` is the good row it is, or the misroute that the same shape
   * over `Reader` would be.
   *
   * One instance in this tree carries it: `Writer.byValue.writerK`,
   * whose test is `Typeable[W]` on the told value — an OPT-IN
   * (`import okay.Writer.byValue.given`), since the default `writerK`
   * tests the class of `Say` alone and pays no E092 for it. Marking a
   * test that is NOT finer
   * than the class defeats the check for that signature, so mark it
   * only after reading the `unapply`.
   */
  trait ByValue[F[_]] extends TypeableK[F]

  /**
   * `enum Users[+A] derives TypeableK` — the instance every effect
   * needs, written by the compiler.
   *
   * The erasure of F is what the test is, and the macro reads it off
   * the type — so this is the hand-written `typeableK(classOf[Users[?]])`
   * with the class no longer spelled out AND compiled to a constant
   * `instanceof` rather than read from a field (see `okay.macros.AnswersMacros.derivedImpl`) —
   * same totality (see `typeableK`: complete when the answer type is
   * the signature's only parameter, partial for `State % S` and
   * friends, which say so themselves).
   */
  inline def derived[F[_]]: TypeableK[F] =
    ${ okay.macros.AnswersMacros.derivedImpl[F] }

  /** `Effect.derived`'s half of the same macro: the class is an
   * `Effect` already, so `derives Effect` needs no wrapper around a
   * `TypeableK` (it had one — `Effect.of(TypeableK.derived)` — which
   * put two virtual calls under every `split`) */
  inline def derivedEffect[F[_]]: Effect[F] =
    ${ okay.macros.AnswersMacros.derivedImpl[F] }

/**
 * WHAT A SIGNATURE SAYS ABOUT ITSELF: `enum Users[+A] derives Effect`.
 *
 * One word, and it reads as what it is — a declaration that this type
 * is an effect signature — where `derives TypeableK` reads as a
 * mechanism. What it currently carries is exactly the mechanism: a
 * row is an untagged union, unions erase, and a handler meeting an
 * operation in `F + G` decides by class test. `Effect` IS that test
 * (it extends `TypeableK`), so everything that asks for one finds
 * this instance in the signature's own companion.
 *
 * When `okay-direct` is present, its bridge exposes this same evidence
 * as the marker that lets a signature's
 * operations auto-color inside a `direct` block:
 *
 *     val prog: Option[String] ! Users = direct {
 *       val old: Option[String] = find(7)   // no mark
 *       old
 *     }
 *
 * That marker was originally a separate, per-project decision
 * (specs/direct-auto-coloring.md): auto-coloring is invasive, so
 * arbitrary `G[A]`s must never silently color. Bundling it moves the
 * decision to the signature's author — which is the operator's call
 * (2026-09-08) and is defensible on its own terms: `derives Effect`
 * is not arbitrary, it is a type declaring that its values ARE
 * operations, which is exactly the claim the marker wanted. The other
 * gate is untouched and does the heavier work: the conversion needs
 * `DirectCtx[F]`, which exists ONLY inside a direct block, so nothing
 * colors anywhere else. An effect that wants the row-split test and
 * NOT auto-coloring writes `derives TypeableK` instead.
 *
 * It is a trait rather than a type alias so that it has room. What
 * joins it has to be DERIVABLE from the declaration alone, which
 * rules out most things and is the point.
 */
trait Effect[F[_]] extends TypeableK[F], DirectEffect[F]

object Effect:
  /** delegates to `TypeableK`'s macro, which is where the check lives
   * that refuses a row */
  inline def derived[F[_]]: Effect[F] =
    TypeableK.derivedEffect[F]

  /**
   * THE class test over a RUN-TIME class: what `typeableK(cls)`
   * builds. A derived signature (`derives Effect`/`TypeableK`) no
   * longer uses it — its test is a constant-class `instanceof` in a
   * class of its own (typeablek-instanceof), which this cannot be:
   * `cls` is a field.
   */
  final class ByClass[F[_]](cls: Class[?]) extends Effect[F]:
    def test(x: Any): Boolean = cls.isInstance(x)

  /** an `Effect` over a test that is NOT by class — `Instances.of`
   * and `Tag.of` read a key or an inner operation. Not inlined,
   * deliberately: an anonymous class in an inline body is duplicated
   * at every derivation site */
  def of[F[_]](t: TypeableK[F]): Effect[F] = new Effect[F]:
    def test(x: Any): Boolean = t.test(x)

@implicitNotFound("no Answers[${F}].\nAn Answers[F] answers each operation of F with a plain value (trait Answers: def handle[A](a: F[A]): A).\nFor a ROW of the classic, build the union from the parts: given Answers[F + G] = Row.union[F, G]\n(each part needs its own Answers in scope first).")
trait Answers[F[_]]:
  def handle[A](a: F[A]): A

extension [F[_]](h: Answers[F])
  /**
   * Every handler can be a recording one, without being written
   * twice.
   *
   *     rename(7, "grace").runWith(using live(c).tracing(log += _))
   *
   * "What did this program ask for, and in what order" is the
   * question a test wants answered, and the operations are ALREADY
   * data — so the answer is a decorator, not a second handler. It
   * sees exactly what the real one sees, because it IS the real one
   * with a line in front.
   */
  def tracing(log: Any => Unit): Answers[F] = new:
    def handle[A](a: F[A]): A = { log(a); h.handle(a) }

final class ComonadAnswers[F[_]](val C: Comonad[F]) extends Answers[F]:
  inline def handle[A](a: F[A]): A = C.extract(a)

given [F[_] : Comonad as C]: Answers[F] = ComonadAnswers[F](C)

object Answers
