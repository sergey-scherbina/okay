package okay.cont

/**
 * FREE WITH ROWS, OVER `Cont` (specs/freer-min.md, stages 27 and 46): `Free[R, A]` is a program over the ROW `R` —
 * the effects it may perform, a list, nominal: `Ask +: Say +: Pure` — built with no handler in sight, as a
 * FUNCTION of a context whose capabilities reach `R`, into `Cont`. THE ROW IS FIXED: `flatMap` binds at one row,
 * and a run finds every operation's capability by its path in the ONE `Has` of the run (stage 46; a join of rows
 * at every bind, `++` with its `Shape.split`, cost 17 ns an operation, stage 43). An operation is `Op`, a program
 * over ANY row that has its effect: its row comes from the expected type where it is bound, as the classic's
 * `effect` takes its signature, so a `for` over mixed effects declares the program's row and nothing else;
 * `handle` takes an effect off the row WHEREVER it is (`Removed`): the order of effects in a type says nothing, the
 * order of handlers everything; `widen` goes to any row that has every effect of this one (`Sub`), written.
 * Nominal lists, not unions: a union in a type lambda unifies by subtyping, loosely — `Member[Cnt, Ask + (Say +
 * Cnt)]` came out as `Cnt + Any` — and a list's `Pure` and `+:` are two classes, so every walk here is total for
 * the compiler, no claim, no cast, no runtime test of an operation's class.
 */
sealed trait Row
/** an effect in front of a row; right-associative: `Ask +: Say +: Pure` */
final class +:[E[+_], T <: Row] extends Row
/** the empty row, the end of every row: a program over it performs nothing */
sealed trait Pure extends Row
/** A ROW AS THE CLASSIC UNION of its effects' operations: `Union[A +: B +: Pure, X]` is `A[X] | B[X]`. A union
 * is commutative, so two rows with the same effects in any order have ONE union — that is what makes them one
 * type of program (`Free.reordered`), and it is the row the classic `!` writes for the same program */
type Union[R <: Row, X] = R match
  case Pure => Nothing
  case e +: t => e[X] | Union[t, X]

/** `E` is in the row `R`: a path to it, the compiler builds it — the first, by priority */
enum Member[E[+_], R <: Row]:
  case Head[E[+_], T <: Row]() extends Member[E, E +: T]
  case Tail[E[+_], E2[+_], T <: Row](m: Member[E, T]) extends Member[E, E2 +: T]
object Member extends MemberLow:
  given head[E[+_], T <: Row]: Member[E, E +: T] = Head()
sealed trait MemberLow:
  given tail[E[+_], E2[+_], T <: Row](using m: Member[E, T]): Member[E, E2 +: T] = Member.Tail(m)

/** AN OPERATION WITH ITS PATH in the row: what a program over `R` holds where its operations are data and not
 * performed at once (the classic tree under the facade, okay.freer); `route` (`Removed`) tells a handler's
 * operations from the rest's by the path, no class test */
sealed trait Tagged[R <: Row, +X]:
  /** performed at a context, by its path — a method, not a match: a match on `At` binds a fresh `X` under the
   * covariance, which the compiler closes at `Nothing` and casts (a ClassCastException at the next frame) */
  def perform(c: Ctx)(has: Has[R, c.type]): Cont[c.Here, c.Here, X]
object Tagged:
  final case class At[E[+_], R <: Row, X](op: E[X], path: Member[E, R]) extends Tagged[R, X]:
    def perform(c: Ctx)(has: Has[R, c.type]): Cont[c.Here, c.Here, X] = has.at(path).perform(op, c)

/** the effects of a row, each with its path: the row as a value, the compiler builds it where the row is known */
enum Members[R <: Row]:
  case MNil() extends Members[Pure]
  case MCons[E[+_], T <: Row](tail: Members[T]) extends Members[E +: T]
object Members:
  given nil: Members[Pure] = MNil()
  given cons[E[+_], T <: Row](using t: Members[T]): Members[E +: T] = MCons(t)

/** a CAPABILITY: how `E` is performed in the context `C` — answered in place, or reached by a capture; and the
 * same capability in a context one level inside, the operation reached from there */
enum Cap[E[+_], C <: Ctx]:
  case Answers[E[+_], C <: Ctx](a: Answered[E, C]) extends Cap[E, C]
  case Reaching[E[+_], C <: In[?, ?]](r: Reaches[E, C]) extends Cap[E, C]
  def perform[X](op: E[X], c: C): Cont[c.Here, c.Here, X] = this match
    case Answers(a) => Cont.Answer[c.Here, X, E](op, a.at(c))
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

/** `E` in the row `R`, BY ITS POSITION: the row without it, `Out`, and the capabilities of `R` from a capability
 * of `E` put at that position into those of `Out` — so a handler takes its effect off WHEREVER it is in the
 * row. The order of effects in a type says nothing; the order of handlers says everything. The compiler builds
 * it: at the head (`here`, first by priority), or through the tail (`there`) */
trait Removed[E[+_], R <: Row]:
  type Out <: Row
  def insert[C <: Ctx](cap: Cap[E, C], rest: Has[Out, C]): Has[R, C]
  /** a tagged operation of `R`: the handler's, or the rest's with its path in `Out` — by the path, total */
  def route[X](t: Tagged[R, X]): Either[E[X], Tagged[Out, X]]
  /** the rest, as a value */
  def rest: Members[Out]
object Removed extends RemovedLow:
  /** the proof with its `Out` named, what a given declares, so a `handle`'s result row is known at its call */
  type Aux[E[+_], R <: Row, O <: Row] = Removed[E, R] { type Out = O }
  given here[E[+_], T <: Row](using ms: Members[T]): Aux[E, E +: T, T] = new Removed[E, E +: T]:
    type Out = T
    def insert[C <: Ctx](cap: Cap[E, C], rest: Has[T, C]): Has[E +: T, C] = Has.HCons(cap, rest)
    def route[X](t: Tagged[E +: T, X]): Either[E[X], Tagged[T, X]] = t match
      case Tagged.At(op, path) => path match
        case Member.Head() => Left(op)
        case Member.Tail(p) => Right(Tagged.At(op, p))
    def rest: Members[T] = ms
sealed trait RemovedLow:
  given there[E[+_], H[+_], T <: Row, O <: Row](using r: Removed.Aux[E, T, O]): Removed.Aux[E, H +: T, H +: O] = new Removed[E, H +: T]:
    type Out = H +: O
    def insert[C <: Ctx](cap: Cap[E, C], rest: Has[H +: O, C]): Has[H +: T, C] = rest match
      case Has.HCons(ch, t) => Has.HCons(ch, r.insert(cap, t))
    def route[X](t: Tagged[H +: T, X]): Either[E[X], Tagged[H +: O, X]] = t match
      case Tagged.At(op, path) => path match
        case Member.Head() => Right(Tagged.At(op, Member.Head()))
        case Member.Tail(p) => r.route(Tagged.At(op, p)) match
          case Left(e) => Left(e)
          case Right(Tagged.At(op2, p2)) => Right(Tagged.At(op2, Member.Tail(p2)))
    def rest: Members[H +: O] = Members.MCons(r.rest)

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
  /** at the one row: nothing joined, nothing split */
  def flatMap[B](f: A => Free[R, B]): Free[R, B] = new Free[R, B]:
    def run(using c: Ctx, has: Has[R, c.type]): Cont[c.Here, c.Here, B] =
      Free.this.run(using c, has).flatMap(a => f(a).run(using c, has))
  def map[B](f: A => B): Free[R, B] = new Free[R, B]:
    def run(using c: Ctx, has: Has[R, c.type]): Cont[c.Here, c.Here, B] = Free.this.run(using c, has).map(f)
  /** at a row that has every effect of this one */
  def widen[R2 <: Row](using sub: Sub[R, R2]): Free[R2, A] = new Free[R2, A]:
    def run(using c: Ctx, has: Has[R2, c.type]): Cont[c.Here, c.Here, A] = Free.this.run(using c, sub(has))
object Free:
  /** THE SAME EFFECTS IN ANOTHER ORDER ARE ONE TYPE OF PROGRAM: where a program over `R2` is expected and one over
   * `R1` is given, the compiler widens it by `Sub` when the rows' unions are one — at `Any`, the top of every
   * effect (a row's effects are covariant), where `State % Int` and `State % String` still differ. A row with
   * FEWER effects is not widened silently: that is `widen`, written */
  given reordered[R1 <: Row, R2 <: Row, A](using same: Union[R1, Any] =:= Union[R2, Any], sub: Sub[R1, R2]): Conversion[Free[R1, A], Free[R2, A]] =
    p => { val _ = same; p.widen[R2](using sub) }

  /** a value, at any row */
  def pure[R <: Row, A](a: A): Free[R, A] = new Free[R, A]:
    def run(using c: Ctx, has: Has[R, c.type]): Cont[c.Here, c.Here, A] = Cont.Return(a)
  /** a program built when the machine reaches it: a tail call between mutually recursive functions costs no frame */
  def delay[R <: Row, A](thunk: () => Free[R, A]): Free[R, A] = new Free[R, A]:
    def run(using c: Ctx, has: Has[R, c.type]): Cont[c.Here, c.Here, A] = Cont.Delay(() => thunk().run(using c, has))
  /** a bind whose left side is deferred */
  def defer[R <: Row, A, B](thunk: () => Free[R, A])(f: A => Free[R, B]): Free[R, B] = delay(thunk).flatMap(f)
  /** an operation: a program over any row that has its effect (`Op`) */
  def inject[E[+_], X](op: E[X]): Op[E, X] = Op(op)
  /** the effect `E` handled, wherever it is in the row: the body under the handler's delimiter, its capability at
   * `E`'s position, the rest's capabilities one level in; the result over the row without `E` */
  def handle[E[+_], A, Ans, R <: Row](h: Handler[E, A, Ans])(p: Free[R, A])(using rm: Removed[E, R]): Free[rm.Out, Ans] =
    new Free[rm.Out, Ans]:
      def run(using c: Ctx, has: Has[rm.Out, c.type]): Cont[c.Here, c.Here, Ans] =
        okay.cont.handle(h)(using c): inner ?=>
          p.run(using inner, rm.insert(Cap.Reaching(Reaches.here[E, Ans, inner.type]), has.lift[inner.type]))
  def handle[E[+_], A, Ans, R <: Row](h: Answering[E, A, Ans])(p: Free[R, A])(using rm: Removed[E, R]): Free[rm.Out, Ans] =
    new Free[rm.Out, Ans]:
      def run(using c: Ctx, has: Has[rm.Out, c.type]): Cont[c.Here, c.Here, Ans] =
        okay.cont.handle(h)(using c): inner ?=>
          p.run(using inner, rm.insert(Cap.Answers(Answered.here[E, inner.type]), has.lift[inner.type]))
  /** a program with nothing left to handle, at the top */
  def top[A](p: Free[Pure, A]): Top[A] = p.run(using Root, Has.HNil())

/**
 * AN OPERATION AS A PROGRAM OVER ANY ROW THAT HAS ITS EFFECT: the row is not the operation's to say — it is the
 * program's it is bound into, so `flatMap` and `map` take it from the expected type (`Member`, the path the
 * compiler builds), and a bare operation becomes a `Free[R, X]` by the same path where one is expected (`at`,
 * or the conversion). Bound, it performs through the one `Has` of the run, at its path: no program built around
 * it, no row joined.
 */
final class Op[E[+_], X](val op: E[X]):
  def flatMap[R <: Row, B](f: X => Free[R, B])(using m: Member[E, R]): Free[R, B] = new Free[R, B]:
    def run(using c: Ctx, has: Has[R, c.type]): Cont[c.Here, c.Here, B] =
      has.at(m).perform(op, c).flatMap(x => f(x).run(using c, has))
  def map[R <: Row, B](f: X => B)(using m: Member[E, R]): Free[R, B] = new Free[R, B]:
    def run(using c: Ctx, has: Has[R, c.type]): Cont[c.Here, c.Here, B] = has.at(m).perform(op, c).map(f)
  /** the operation as a program at a row, written */
  def at[R <: Row](using m: Member[E, R]): Free[R, X] = new Free[R, X]:
    def run(using c: Ctx, has: Has[R, c.type]): Cont[c.Here, c.Here, X] = has.at(m).perform(op, c)
object Op:
  /** a bare operation where a program over `R` is expected */
  given toFree[E[+_], X, R <: Row](using m: Member[E, R]): Conversion[Op[E, X], Free[R, X]] = _.at[R]
