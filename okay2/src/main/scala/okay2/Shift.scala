package okay2

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

/**
 * A continuation as an effect (the Scala 3 core's `Shift % R`, specs/shift-effect.md): `Shift[R]` in the row
 * is a capture to the nearest `reset` of answer `R`, and `reset` is its handler. Its operations are Delim's
 * and run on Delim's machine; no value of this type is made. Scala 2 has no context functions, so there is no
 * short `shift[A]` inside a `reset { }`: a capture names its answer, value and row, as `Delim.shift` does.
 */
sealed trait Shift[R] extends Row with Shift.Any { type Op[+A] = Delim.Op[A] }

/** the level-1 doors, mixed into the package object: `shift`, `shift0`, `reset` */
trait Shifts {

  /** Danvy-Filinski's capture: the body runs under its `reset`, so it may capture to it again; `k` re-installs it */
  def shift[R, A, F <: Row](f: (A => R ! (Shift[R] + F)) => R ! (Shift[R] + F))(implicit k: Shift.Key[R], at: At): A ! (Shift[R] + F) =
    Shift.out[A, R, F](Delim.shift[R, A, F](k.prompt)(Shift.clause[R, A, Shift[R] + F, F](f))(at))

  /** the body runs outside its `reset`; `k` re-installs it */
  def shift0[R, A, F <: Row](f: (A => R ! F) => R ! F)(implicit k: Shift.Key[R], at: At): A ! (Shift[R] + F) =
    Shift.out[A, R, F](Delim.shift0[R, A, F](k.prompt)(Shift.clause[R, A, F, F](f))(at))

  /** delimit, and answer every capture of answer `R` */
  def reset[R, F <: Row](body: R ! (Shift[R] + F))(implicit k: Shift.Key[R], n: Shift.Nesting[F]): R ! F = {
    val pushed = Delim.push[R, F](k.prompt)(Shift.in[R, R, F](body))
    // a row that still holds a capture's effect is run by the machine outside
    if (n.inner) Shift.inner[R, F](pushed) else Shift.run[R, F](pushed)
  }
}

object Shift {
  /** every `Shift[R]`, whatever R: what `Nesting` looks for in a row */
  sealed trait Any extends Row

  // THE NAMED PATTERNS (shift-patterns): the captures most programs want, so an early exit and a generator
  // need no `Delim` in sight. In this object, as in the Scala 3 core: a top-level `collect` would clash.

  /** leave the nearest `reset` of answer `R` now, with its answer `v`: what follows is dropped */
  def exit[R, A, F <: Row](v: R)(implicit k: Key[R], at: At): A ! (Shift[R] + F) =
    okay2.shift0[R, A, F](_ => pure[F, R](v))

  /** a generator: run `body`, and answer everything it `emit`ted, in order */
  def collect[W, F <: Row](body: Unit ! (Shift[List[W]] + F))(implicit k: Key[List[W]], n: Nesting[F]): List[W] ! F =
    okay2.reset[List[W], F](body.map(_ => Nil))

  /** inside `collect`: hand `w` out, and go on */
  def emit[W, F <: Row](w: W)(implicit k: Key[List[W]], at: At): Unit ! (Shift[List[W]] + F) =
    okay2.shift0[List[W], Unit, F](k => k(()).map(w :: _))

  /** `reset` as a value, for the handler-value doors */
  def handle[R, F <: Row](p: R ! (Shift[R] + F))(implicit k: Key[R], n: Nesting[F]): R ! F = okay2.reset[R, F](p)

  /** level 2: the program as a `Cont` whose answers are programs: `c / k` is `reset(q >>= k)` */
  def cont[A, R, F <: Row](q: A ! (Shift[R] + F))(implicit k: Key[R], n: Nesting[F]): Cont[A, R ! F, R ! F] =
    Cont.shift[A, R ! F, R ! F](kk => okay2.reset[R, F](q.flatMap[Shift[R] + F, R](a => kk(a))))

  /** level 2: a whole `Cont` as one capture */
  def embed[A, R, F <: Row](c: Cont[A, R ! F, R ! F])(implicit k: Key[R], at: At): A ! (Shift[R] + F) =
    okay2.shift0[R, A, F](kk => c / kk)

  /**
   * A `reset` that runs its own machine runs it INSIDE whatever forced it, and nested resets of one answer type
   * each start one: JVM depth grows with the nesting. So the runs are counted per thread, and past the room the
   * next one runs on a fresh stack, as Cont's strict `k` does (StackSwitch, specs/cont-stack.md Layer 2). A
   * level is taken as ~4 KB cold, Cont's ~1.2 KB scaled. The Scala 3 core's twin.
   */
  private val room: Int = Integer.getInteger("okay.shift.room", math.max(32L, StackSwitch.firstRoom.toLong * 1200 / 4096).toInt)
  private val left: ThreadLocal[Array[Int]] = new ThreadLocal[Array[Int]] { override def initialValue(): Array[Int] = Array(room) }

  /** run the machine for one `reset`, one level less of room; at zero on a fresh stack */
  private[okay2] def run[R, F <: Row](pushed: R ! (Delim + F)): R ! F = {
    val cell = left.get
    val here = cell(0)
    if (here > 0) {
      cell(0) = here - 1
      try machine[R, F](pushed) finally cell(0) = here
    } else StackSwitch.fresh { big =>
      val c = left.get
      val saved = c(0)
      c(0) = big / 2
      try machine[R, F](pushed) finally c(0) = saved
    }
  }

  // Delim.run's `OneMachine` asks the row to be free of Delim, which `Nesting` already decided at the door
  private def machine[R, F <: Row](pushed: R ! (Delim + F)): R ! F = Delim.run[R, F](pushed)(Delim.OneMachine.unchecked[F])

  // THE ONE CLAIM: a `Shift[R]` program is a `Delim` program at the same erasure (only the machine reads its
  // operations), and a capture of answer `R` reaches only the prompt of `R`'s key, where its `k` and body are
  // typed in that `reset`'s row.
  private[okay2] def in[A, R, F <: Row](q: A ! (Shift[R] + F)): A ! (Delim + F) = q.asInstanceOf[A ! (Delim + F)]
  private[okay2] def out[A, R, F <: Row](q: A ! (Delim + F)): A ! (Shift[R] + F) = q.asInstanceOf[A ! (Shift[R] + F)]
  private[okay2] def inner[R, F <: Row](q: R ! (Delim + F)): R ! F = q.asInstanceOf[R ! F]
  private[okay2] def clause[R, A, G <: Row, F <: Row](f: (A => R ! G) => R ! G): (A => R ! (Delim + F)) => R ! (Delim + F) =
    f.asInstanceOf[(A => R ! (Delim + F)) => R ! (Delim + F)]

  /** the test reads the prompt, so `Shift[Int] + Shift[String]` is a good row */
  implicit def typeableK[R](implicit k: Key[R]): TypeableK.ByValue[Shift[R]] = new TypeableK.ByValue[Shift[R]] {
    def test(x: scala.Any): Boolean = x match {
      case c: Delim.Capture[_, _] => c.prompt eq k.prompt
      case p: Delim.Push[_] => p.prompt eq k.prompt
      case d: Delim.Dollar[_, _] => d.prompt eq k.prompt
      case _ => false
    }
  }

  /**
   * The key of an answer type, made at compile time: two types, two keys; one type (through any alias, an
   * intersection in either order), one key, and one prompt for it. An abstract type has none: it is passed in,
   * as a `ClassTag` is.
   */
  final class Key[R] private (val id: String, private[okay2] val prompt: Prompt[R]) {
    override def toString: String = id
  }

  object Key {
    private val keys = new java.util.concurrent.ConcurrentHashMap[String, Key[scala.Any]]

    /** the key of `id`, one per id */
    def intern[R](id: String): Key[R] = {
      val k = keys.get(id)
      // one key per id, made at `Any` and read back at the type the id names
      (if (k != null) k else keys.computeIfAbsent(id, i => new Key[scala.Any](i, new Prompt[scala.Any]("reset", i)))).asInstanceOf[Key[R]]
    }

    implicit def of[R]: Key[R] = macro ShiftMacro.key[R]
  }

  /**
   * Whether a row still holds a capture's effect (a `Shift` or `Delim`): a `reset` over such a row pushes its
   * prompt on the machine an outer one runs. An abstract row reads as none, so code generic in the row and
   * nested in another `reset` passes its `Nesting` on.
   */
  final class Nesting[F <: Row] private[okay2] (val inner: Boolean)

  object Nesting extends DelimNesting {
    implicit def shifting[F <: Row](implicit ev: F <:< Shift.Any): Nesting[F] = { val _ = ev; new Nesting[F](true) }
  }
  trait DelimNesting extends PlainNesting {
    implicit def delimiting[F <: Row](implicit ev: F <:< Delim): Nesting[F] = { val _ = ev; new Nesting[F](true) }
  }
  trait PlainNesting {
    implicit def plain[F <: Row]: Nesting[F] = new Nesting[F](false)
  }
}

object ShiftMacro {
  def key[R: c.WeakTypeTag](c: blackbox.Context): c.Tree = {
    import c.universe._
    val r = weakTypeOf[R]
    // the members of an intersection, flattened by a worklist
    def parts(t: Type): List[Type] = {
      val out = List.newBuilder[Type]
      var todo = List(t)
      while (todo.nonEmpty) {
        val h = todo.head
        todo = todo.tail
        h.dealias match {
          case RefinedType(ps, _) => todo = ps ++ todo
          case other => out += other
        }
      }
      out.result()
    }
    // bounded by the type's own nesting, which the compiler has already walked
    def norm(t: Type): String = t.dealias match {
      case rt: RefinedType => parts(rt).map(norm).distinct.sorted.mkString("(", " with ", ")")
      case ConstantType(Constant(v)) => v.toString
      case TypeRef(_, sym, args) if sym.isClass =>
        sym.fullName + (if (args.isEmpty) "" else args.map(norm).mkString("[", ", ", "]"))
      case SingleType(_, sym) => sym.fullName + ".type"
      case other => c.abort(c.enclosingPosition,
        s"the answer type $r is abstract here ($other), so a reset or shift of it has no key; " +
          s"take a `Shift.Key[$other]` as a parameter where the type is known")
    }
    q"_root_.okay2.Shift.Key.intern[$r](${norm(r)})"
  }
}
