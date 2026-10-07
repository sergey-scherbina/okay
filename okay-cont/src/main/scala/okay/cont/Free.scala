package okay.cont

/**
 * FREE WITH ROWS, OVER `Cont` (specs/freer-min.md, stage 27): `Free[R, A]` is a program over the ROW `R` — the
 * effects it may perform, a list, nominal: `Ask +: Say +: Pure` — built with no handler in sight, as a FUNCTION of
 * a context whose capabilities reach `R`, into `Cont`. `inject` needs its effect in the row (`Member`, a path the
 * compiler builds); `flatMap` joins the rows (`++`, a match type) and `run` splits the capabilities by the first
 * row's shape (`Shape`, a proof the compiler builds); `handle` takes the head effect off the row, giving the body
 * the handler's context and lifting the rest's capabilities one level in; `widen` goes to any row that has every
 * effect of this one (`Sub`). Nominal lists, not unions: a union in a type lambda unifies by subtyping, loosely —
 * `Member[Cnt, Ask + (Say + Cnt)]` came out as `Cnt + Any` — and a list's `Pure` and `+:` are two classes, so
 * every walk here is total for the compiler, no claim, no cast, no runtime test of an operation's class.
 */
sealed trait Row
/** an effect in front of a row; right-associative: `Ask +: Say +: Pure` */
final class +:[E[+_], T <: Row] extends Row
/** the empty row, the end of every row: a program over it performs nothing */
sealed trait Pure extends Row
/** the join of two rows, `flatMap`'s */
type ++[R1 <: Row, R2 <: Row] <: Row = R1 match
  case Pure => R2
  case e +: t => e +: (t ++ R2)

/** `E` is in the row `R`: a path to it, the compiler builds it — the first, by priority */
enum Member[E[+_], R <: Row]:
  case Head[E[+_], T <: Row]() extends Member[E, E +: T]
  case Tail[E[+_], E2[+_], T <: Row](m: Member[E, T]) extends Member[E, E2 +: T]
object Member extends MemberLow:
  given head[E[+_], T <: Row]: Member[E, E +: T] = Head()
sealed trait MemberLow:
  given tail[E[+_], E2[+_], T <: Row](using m: Member[E, T]): Member[E, E2 +: T] = Member.Tail(m)

/** a CAPABILITY: how `E` is performed in the context `C` — answered in place, or reached by a capture; and the
 * same capability in a context one level inside, the operation reached from there */
enum Cap[E[+_], C <: Ctx]:
  case Answers[E[+_], C <: Ctx](a: Answered[E, C]) extends Cap[E, C]
  case Reaching[E[+_], C <: In[?, ?]](r: Reaches[E, C]) extends Cap[E, C]
  def perform[X](op: E[X], c: C): Cont[c.Here, c.Here, X] = this match
    case Answers(a) => Cont.Answer[c.Here, X, E, C](op, c, a)
    case Reaching(r) =>
      val t = r.target(c)
      Cont.Op[c.Here, X, t.Dn, t.Ansn, E](t.reach, op, t.clause)
  def lift[C2 <: In[?, C]]: Cap[E, C2] = this match
    case Answers(a) => Answers(Answered.outside[E, C, C2](using a))
    case Reaching(r) => Reaching(Reaches.out[E, C, C2](using r))
object Cap extends CapLow:
  given answers[E[+_], C <: Ctx](using a: Answered[E, C]): Cap[E, C] = Answers(a)
sealed trait CapLow:
  given reaching[E[+_], C <: In[?, ?]](using r: Reaches[E, C]): Cap[E, C] = Cap.Reaching(r)

/** the capabilities of a row in a context: a list the row's shape; the compiler builds it where the row is known */
enum Has[R <: Row, C <: Ctx]:
  case HCons[E[+_], T <: Row, C <: Ctx](cap: Cap[E, C], tail: Has[T, C]) extends Has[E +: T, C]
  case HNil[C <: Ctx]() extends Has[Pure, C]
  /** the capability of `E`, by its path: total, the list and the path are of one shape */
  def at[E[+_]](m: Member[E, R]): Cap[E, C] = m match
    case Member.Head() => this match
      case HCons(cap, _) => cap
    case Member.Tail(m2) => this match
      case HCons(_, t) => t.at(m2)
  /** the same capabilities one level inside */
  def lift[C2 <: In[?, C]]: Has[R, C2] = this match
    case HCons(cap, t) => HCons(cap.lift[C2], t.lift[C2])
    case HNil() => HNil()
object Has:
  given cons[E[+_], T <: Row, C <: Ctx](using cap: Cap[E, C], t: Has[T, C]): Has[E +: T, C] = HCons(cap, t)
  given nil[C <: Ctx]: Has[Pure, C] = HNil()

/** the shape of a row, a proof the compiler builds; `split` in each case, where the row is the case's own
 * constructor and the join reduces on it */
sealed trait Shape[R <: Row]:
  def split[R2 <: Row, C <: Ctx](h: Has[R ++ R2, C]): (Has[R, C], Has[R2, C])
final case class SNil() extends Shape[Pure]:
  def split[R2 <: Row, C <: Ctx](h: Has[Pure ++ R2, C]): (Has[Pure, C], Has[R2, C]) = (Has.HNil(), h)
final case class SCons[E[+_], T <: Row](t: Shape[T]) extends Shape[E +: T]:
  def split[R2 <: Row, C <: Ctx](h: Has[(E +: T) ++ R2, C]): (Has[E +: T, C], Has[R2, C]) = h match
    case Has.HCons(cap, rest) =>
      val (a, b) = t.split[R2, C](rest)
      (Has.HCons(cap, a), b)
object Shape:
  given nil: Shape[Pure] = SNil()
  given cons[E[+_], T <: Row](using t: Shape[T]): Shape[E +: T] = SCons(t)

/** every effect of `R1` is in `R2`: the capabilities of `R1` from those of `R2` */
trait Sub[R1 <: Row, R2 <: Row]:
  def apply[C <: Ctx](h: Has[R2, C]): Has[R1, C]
object Sub:
  given nil[R2 <: Row]: Sub[Pure, R2] with
    def apply[C <: Ctx](h: Has[R2, C]): Has[Pure, C] = Has.HNil()
  given cons[E[+_], T <: Row, R2 <: Row](using m: Member[E, R2], t: Sub[T, R2]): Sub[E +: T, R2] with
    def apply[C <: Ctx](h: Has[R2, C]): Has[E +: T, C] = Has.HCons(h.at(m), t(h))

/** a program over the row `R`: a function of a context whose capabilities reach `R`, into `Cont` */
trait Free[R <: Row, +A]:
  def run(using c: Ctx, has: Has[R, c.type]): Cont[c.Here, c.Here, A]
  /** the rows joined; this row's shape splits the capabilities */
  def flatMap[R2 <: Row, B](f: A => Free[R2, B])(using s: Shape[R]): Free[R ++ R2, B] = new Free[R ++ R2, B]:
    def run(using c: Ctx, has: Has[R ++ R2, c.type]): Cont[c.Here, c.Here, B] =
      val (mine, theirs) = s.split[R2, c.type](has)
      Free.this.run(using c, mine).flatMap(a => f(a).run(using c, theirs))
  def map[B](f: A => B): Free[R, B] = new Free[R, B]:
    def run(using c: Ctx, has: Has[R, c.type]): Cont[c.Here, c.Here, B] = Free.this.run(using c, has).map(f)
  /** at a row that has every effect of this one */
  def widen[R2 <: Row](using sub: Sub[R, R2]): Free[R2, A] = new Free[R2, A]:
    def run(using c: Ctx, has: Has[R2, c.type]): Cont[c.Here, c.Here, A] = Free.this.run(using c, sub(has))
object Free:
  def pure[A](a: A): Free[Pure, A] = new Free[Pure, A]:
    def run(using c: Ctx, has: Has[Pure, c.type]): Cont[c.Here, c.Here, A] = Cont.Return(a)
  /** an operation: its effect alone is the row */
  def inject[E[+_], X](op: E[X]): Free[E +: Pure, X] = new Free[E +: Pure, X]:
    def run(using c: Ctx, has: Has[E +: Pure, c.type]): Cont[c.Here, c.Here, X] = has match
      case Has.HCons(cap, _) => cap.perform(op, c)
  /** the head effect handled: the body under the handler's delimiter, the rest's capabilities one level in */
  def handle[E[+_], A, Ans, R <: Row](h: Handler[E, A, Ans])(p: Free[E +: R, A]): Free[R, Ans] = new Free[R, Ans]:
    def run(using c: Ctx, has: Has[R, c.type]): Cont[c.Here, c.Here, Ans] =
      okay.cont.handle(h)(using c): inner ?=>
        p.run(using inner, Has.HCons(Cap.Reaching(Reaches.here[E, Ans, inner.type]), has.lift[inner.type]))
  def handle[E[+_], A, Ans, R <: Row](h: Answering[E, A, Ans])(p: Free[E +: R, A]): Free[R, Ans] = new Free[R, Ans]:
    def run(using c: Ctx, has: Has[R, c.type]): Cont[c.Here, c.Here, Ans] =
      okay.cont.handle(h)(using c): inner ?=>
        p.run(using inner, Has.HCons(Cap.Answers(Answered.here[E, inner.type]), has.lift[inner.type]))
  /** a program with nothing left to handle, at the top */
  def top[A](p: Free[Pure, A]): Top[A] = p.run(using Root, Has.HNil())
