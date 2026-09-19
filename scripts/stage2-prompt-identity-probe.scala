//> using scala 3.9.0

/** freer-base STAGE 2, the Delim half: the language question, settled
 *  by compiling. Run it — it is a whole answer in one file:
 *
 *      scala-cli run scripts/stage2-prompt-identity-probe.scala
 *
 *  THE QUESTION. `Delim.Prompt[R]` is made at RUN time (`new Prompt[R]`
 *  inside `reset`), and `Delim.scala:223` throws `NoPrompt` when a
 *  shift names a prompt that is not installed. Stage 2 asks whether
 *  that throw can become a compile error — which needs a prompt's
 *  IDENTITY, not just its answer type, to reach the type level.
 *
 *  THE ANSWER IS YES, and this file is the evidence: five positives
 *  compile, and the three negatives are refused by the compiler,
 *  including the escaped-prompt case that is exactly today's throw.
 *
 *  FOUR COMPILER FACTS were paid for on the way, each by a failing
 *  round, and they are why the shape below looks the way it does:
 *
 *  1. An expected type fixes the indexes, but the HEAD of a
 *     for-comprehension has none: `x.flatMap(...)` types `x` first, so
 *     `S` fell back to its bound `Tuple` and the search failed. THIS
 *     is why the stack is a given rather than an inferred parameter.
 *  2. A CURRIED dependent context function — `(p: Prompt[R]) =>
 *     Stack[p.type *: S] ?=> Prog[...]`, the obvious way to hand the
 *     body both the prompt and the stack — is refused outright:
 *     "Implementation restriction ... not yet supported".
 *  3. A NON-curried one compiles, and nested witnesses of the same
 *     shape resolve to the INNER one with no ambiguity (which the
 *     nested case below needs). But carrying the stack through it
 *     CRASHES the compiler: `java.lang.AssertionError: wildApprox
 *     failed to remove uninstantiated R`, in implicit scope
 *     computation. Do not take that road.
 *  4. A `using` clause placed AFTER the continuation loses: the
 *     lambda is typed first and pins the stack to `Tuple`. It goes
 *     BEFORE, and the stack is a type MEMBER (`Stack#S`) so no call
 *     site ever spells it.
 *
 *  WHAT IT COSTS AT THE CALL SITE. `reset { p => ... }` becomes
 *  `reset { s => import s.given; ... }` — one line — and the prompt is
 *  `s.p`. The type arguments on `shift` are NOT a new cost: TestDelim
 *  already writes `shift[Int, Int, okay.Pure](p)` at every call today.
 *
 *  The tree here is a stub on purpose. `Free` is not the question, and
 *  nothing in this file is proposed as the implementation — the facade
 *  shape is (tree = syntax, index = a claim about syntax, claims live
 *  on facades and are never matched). specs/freer-base.md carries the
 *  reasoning.
 */
object Stage2:

  /** stands in for Free - the tree is not the question */
  enum Tree[+A]:
    case Pure(a: A)
    case Op(tag: String)
    case Bind[X, B](m: Tree[X], f: X => Tree[B]) extends Tree[B]

  /** a delimiter tag, created at RUN time - the whole problem */
  final class Prompt[R]

  /** the stack in force, as a lexical given. The type MEMBER is the
   *  point: a caller never spells the stack, and no method needs a
   *  type parameter that inference could pin to `Tuple` too early. */
  final class Stack[S0 <: Tuple]:
    type S = S0

  /** what `reset` hands its body: the prompt, and the stack that
   *  installing it produced, ready to be imported */
  final class In[R, S <: Tuple](val p: Prompt[R]):
    given stack: Stack[p.type *: S] = new Stack[p.type *: S]

  /** "p is on the stack" - the using clause that replaces
   *  Delim.scala's `throw NoPrompt()` */
  @annotation.implicitNotFound("prompt ${P} is not on the stack ${S}")
  sealed trait Has[S <: Tuple, P]
  object Has:
    given here[P, S <: Tuple]: Has[P *: S, P] = new Has[P *: S, P] {}
    given there[P, Q, S <: Tuple](using Has[S, P]): Has[Q *: S, P] =
      new Has[Q *: S, P] {}

  /** THE FACADE: the tree is syntax, the indexes are a claim about it */
  opaque type Prog[A, S <: Tuple, R <: Tuple] = Tree[A]

  object Prog:

    extension [A, S <: Tuple, R <: Tuple](m: Prog[A, S, R])
      def flatMap[B, T <: Tuple](f: A => Prog[B, R, T]): Prog[B, S, T] =
        Tree.Bind(m, f)
      def map[B](f: A => B): Prog[B, S, R] =
        Tree.Bind(m, (a: A) => Tree.Pure(f(a)))

    /** an ordinary effect leaves the stack alone, and reads it from
     *  the given rather than from an expected type */
    def op[A](tag: String)(using st: Stack[?]): Prog[A, st.S, st.S] =
      Tree.Op(tag)

    def pure[A](x: A)(using st: Stack[?]): Prog[A, st.S, st.S] =
      Tree.Pure(x)

    /** reset: a FRESH prompt, installed for the body only */
    def reset[R](using st: Stack[?])(
        body: (s: In[R, st.S]) => Prog[R, s.p.type *: st.S, s.p.type *: st.S]
    ): Prog[R, st.S, st.S] =
      body(new In[R, st.S](new Prompt[R]))

    /** capture up to `p` - REQUIRES p on the stack in force */
    def shift[R, A](p: Prompt[R])(using st: Stack[?], ev: Has[st.S, p.type])(
        f: (A => Prog[R, st.S, st.S]) => Prog[R, st.S, st.S]
    ): Prog[A, st.S, st.S] =
      Tree.Op("shift")

object Stage2Test:
  import Stage2.*
  import Stage2.Prog.*

  inline def refuses(inline code: String): Boolean =
    !scala.compiletime.testing.typeChecks(code)

  /** the root: every program starts outside every delimiter */
  given Stack[EmptyTuple] = new Stack[EmptyTuple]

  type Top[A] = Prog[A, EmptyTuple, EmptyTuple]

  // ---- 1. POSITIVE: the simple shape
  val simple: Top[Int] = reset { s =>
    import s.given
    shift[Int, Int](s.p)(k => k(1))
  }

  // ---- 2. POSITIVE: a for-comprehension - THE ROUND-2 FAILURE
  val comprehension: Top[Int] = reset { s =>
    import s.given
    for
      a <- shift[Int, Int](s.p)(k => k(1))
      b <- shift[Int, Int](s.p)(k => k(a + 1))
    yield b
  }

  // ---- 3. POSITIVE: an ordinary effect in the HEAD position
  val mixed: Top[Int] = reset { s =>
    import s.given
    for
      a <- op[Int]("read")
      b <- shift[Int, Int](s.p)(k => k(a + 1))
    yield b
  }

  // ---- 4. POSITIVE: nesting, and a shift to the OUTER prompt from
  //         inside the inner one - `Has.there` doing its work, and the
  //         inner `import` winning over the outer one
  val nested: Top[Int] = reset { outer =>
    import outer.given
    reset { inner =>
      import inner.given
      shift[Int, Int](outer.p)(k => k(1))
    }
  }

  // ---- 5. POSITIVE: reset as a step inside a larger program, where
  //         the expected type comes from flatMap, not from a val
  val sequenced: Top[Int] =
    for
      a <- op[Int]("before")
      b <- reset[Int] { s =>
             import s.given
             shift[Int, Int](s.p)(k => k(a))
           }
    yield b

  // ---- 6. NEGATIVE: a shift with NO reset. Today this is
  //         Delim.scala:223 throwing NoPrompt at RUN time.
  val noPrompt = refuses(
    """{ val loose = new Stage2.Prompt[Int]
         Stage2.Prog.shift[Int, Int](loose)(k => k(1)) }"""
  )

  // ---- 7. NEGATIVE: a foreign prompt inside a reset that installed a
  //         different one. Both have type Prompt[Int].
  val foreign = refuses(
    """{ val stolen = new Stage2.Prompt[Int]
         val bad: Stage2Test.Top[Int] = Stage2.Prog.reset { s =>
           import s.given
           Stage2.Prog.shift[Int, Int](stolen)(k => k(1))
         }
         bad }"""
  )

  // ---- 8. NEGATIVE: the prompt ESCAPES its reset and is shifted to
  //         afterwards - the run-time NoPrompt this type exists for
  val escaped = refuses(
    """{ var leaked: Stage2.Prompt[Int] = null
         val a: Stage2Test.Top[Int] = Stage2.Prog.reset { s =>
           import s.given
           leaked = s.p
           Stage2.Prog.shift[Int, Int](s.p)(k => k(1))
         }
         val b: Stage2Test.Top[Int] = Stage2.Prog.shift[Int, Int](leaked)(k => k(1))
         b }"""
  )

  def main(args: Array[String]): Unit =
    println("1 simple reset/shift:       compiled")
    println("2 for-comprehension:        compiled")
    println("3 effect in head position:  compiled")
    println("4 nested, outer prompt:     compiled")
    println("5 reset inside a program:   compiled")
    println(s"6 shift with no reset:      refused = $noPrompt")
    println(s"7 shift to foreign prompt:  refused = $foreign")
    println(s"8 shift to escaped prompt:  refused = $escaped")
    println(if noPrompt && foreign && escaped then "PROBE GREEN" else "PROBE RED")
