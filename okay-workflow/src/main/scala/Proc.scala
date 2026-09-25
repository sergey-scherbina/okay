package okay


/**
 * A PROCEDURE WHOSE EVERY STEP IS KNOWN BEFORE IT RUNS — the free
 * arrow over a signature (specs/static-workflow.md, with the shape
 * fixed by specs/arrows-plan.md Decision 1).
 *
 * `Static` is the free APPLICATIVE over a signature: a program whose
 * effects are known in advance, with no input. This is its arrow —
 * the same bargain one rung up the ladder (Lindley, Wadler & Yallop
 * 2011: idioms are oblivious, arrows are meticulous, monads are
 * promiscuous), where a step may look at what came before it because
 * a value travels along the edges.
 *
 * WHAT IS DELIBERATELY ABSENT IS `ArrowApply`. Hughes (2000, §4.5):
 * an arrow with `app : P[(P[A, B], A), B]` — a computation that
 * receives a computation AS A VALUE and runs it — is exactly a monad,
 * and `Free.Bind`'s `f: A => Free[F, B]` is `app` spelled as a host
 * closure. So the rule for this type is not "no monads": it is NO
 * OPERATION THAT TAKES A COMPUTATION AS DATA. Inside a leaf a monad
 * is welcome and is the point — an activity runs whatever it likes
 * between two journal records.
 *
 * WHAT THAT BUYS, and it is the whole reason the type exists: the
 * term is finite and walkable, so "where does this run stand" is a
 * PATH rather than a closure, "which operations may it reach" is a
 * question answerable before it starts, and "does this journal still
 * fit this program" can be asked of a deploy rather than discovered
 * by one. `Free` gives none of those up cheaply: past a `Bind`,
 * walking IS running.
 *
 * THE ONE CONSTRUCTOR THE LITERATURE'S SELECTIVE LACKS IS `Iter`.
 * Mokhov et al.'s `whileS` is a recursive DEFINITION, which in a
 * strict free structure is an infinite term; Elgot iteration is a
 * NODE — run the body, `Left` goes round, `Right` leaves — so "ask N
 * questions where N is an answer" stays a finite term whose position
 * is a path with a counter on it. It is not `ArrowLoop` (Paterson's
 * lazy value feedback), which cannot say "run the body again".
 */
enum Proc[F[+_], X, Y]:
  /** a pure step: no operation, nothing journalled, re-run on every walk */
  case Arr(f: X => Y)

  /**
   * THE LEAF — one operation of the signature, built from the input.
   *
   * It carries a NAME, and the name is the static content: `Static`'s
   * `leaves` can report operation VALUES because an applicative's
   * leaves exist before the run, and an arrow's do not — the
   * operation here is a function of an input that has not arrived.
   * So what a reader, a picture and a deploy check can have is the
   * name and the shape, which is enough for all three.
   */
  case Op(name: String, run: X => F[Y])

  case Then[F[+_], X, Y, Z](f: Proc[F, X, Y], g: Proc[F, Y, Z]) extends Proc[F, X, Z]

  /** the strength: carry a value past a step untouched */
  case First[F[+_], X, Y, C](f: Proc[F, X, Y]) extends Proc[F, (X, C), (Y, C)]

  /** the choice: run the step on one side of a sum, pass the other
   * through. Named for `Optic.Choice.right`, which is the primitive
   * this library has (a prism's requirement) — the literature's
   * `left` is its mirror. */
  case OnRight[F[+_], X, Y, C](f: Proc[F, X, Y]) extends Proc[F, Either[C, X], Either[C, Y]]

  /** Elgot iteration: `Left` goes round from the new value, `Right` leaves */
  case Iter(body: Proc[F, X, Either[X, Y]])

  /**
   * TWO INDEPENDENT BRANCHES OVER THE SAME INPUT
   * (specs/static-workflow.md, stage 5).
   *
   * Both read `X` — that is what independent MEANS here. A branch
   * that needed the other's answer would be a `Then`, and writing it
   * as a `Par` would be a lie the type system can tell.
   *
   * WHAT IS PARALLEL IS THE WAITING. A durable journal's record says
   * nothing about which question it answers (the driver matches
   * positionally), so the two branches' answers are still RECORDED in
   * term order — left, then right. What the node buys is that both
   * pending questions are known at once, so a front end can put them
   * to two people on the same morning instead of one after the other.
   * `Wf.Proc.walk` answers `Standing.Waiting` with both.
   *
   * It is not `&&&`: an arrow's fanout is derivable from `first` and
   * `compose` and would be a `Then` of plumbing, which walks as ONE
   * position. The node exists precisely so that the position can be a
   * pair.
   */
  case Par[F[+_], X, Y, Z](f: Proc[F, X, Y], g: Proc[F, X, Z]) extends Proc[F, X, (Y, Z)]

  /**
   * A STEP WITH ITS INVERSE BESIDE IT
   * (specs/static-workflow.md, stage 5).
   *
   * `undo` is given what the step was GIVEN and what it PRODUCED —
   * both, because a compensation usually needs the second (the charge
   * id to refund) and sometimes the first (the account to refund it
   * to).
   *
   * UNDER `foldMap` AN `Undo` IS ITS STEP. The inverse is metadata
   * until something fails, and nothing here decides that something
   * has: `Wf.Proc.compensating` walks the term over a journal and
   * builds the undo chain out of the `Undo` nodes that COMPLETED.
   *
   * The undos are FOUND rather than carried, and that is what makes
   * this possible at all: a stack of computations travelling on the
   * edge, to be run later, is `ArrowApply` — the one thing this type
   * refuses. A term is walkable, so the compensation for a step is
   * reachable at the path where the step ran.
   */
  case Undo[F[+_], X, Y](step: Proc[F, X, Y], undo: Proc[F, (X, Y), Unit]) extends Proc[F, X, Y]

object Proc:

  import Path.{/, steps}

  /** one operation as a step — the door in */
  def op[F[+_], X, Y](name: String)(run: X => F[Y]): Proc[F, X, Y] = Op(name, run)

  /** a pure step */
  def arr[F[+_], X, Y](f: X => Y): Proc[F, X, Y] = Arr(f)

  /** iteration, as the door rather than the constructor */
  def iter[F[+_], X, Y](body: Proc[F, X, Either[X, Y]]): Proc[F, X, Y] = Iter(body)

  /** two independent branches over the same input, as the door */
  def par[F[+_], X, Y, Z](f: Proc[F, X, Y], g: Proc[F, X, Z]): Proc[F, X, (Y, Z)] = Par(f, g)

  /** a step with its compensation, as the door */
  def undoable[F[+_], X, Y](step: Proc[F, X, Y])(undo: Proc[F, (X, Y), Unit]): Proc[F, X, Y] =
    Undo(step, undo)

  /**
   * COMPOSITION, WITH THE ONE REWRITE THAT PAYS FOR ITSELF: two pure
   * steps in a row are one pure step.
   *
   * It matters because a `Proc.direct` block emits an `Arr` per
   * statement, and the statements between two leaves are usually all
   * pure — so without this a five-line block would carry a dozen
   * nodes that `leaves` has to skip and `render` has to draw. With
   * it, a term's nodes are its leaves and the plumbing between them.
   * A hand-written term gets the same fold for free.
   */
  def andThen[F[+_], X, Y, Z](f: Proc[F, X, Y], g: Proc[F, Y, Z]): Proc[F, X, Z] =
    (f, g) match
      case (Arr(a), Arr(b)) => Arr(a.andThen(b))
      case _ => Then(f, g)

  /**
   * THE SHAPE EVERY LEAF OF A BLOCK TAKES: run an operation built
   * from the environment, and APPEND its answer to the environment.
   *
   * What a monadic body keeps in a local variable, a term carries on
   * its edge — so a block's environment is a left-nested tuple that
   * grows by one at every bound name, and this is the node that grows
   * it. Written out rather than left to `second`, whose default
   * expansion is a `dimap` over a `first` and costs two more nodes
   * per leaf.
   */
  def keeping[F[+_], E, A](name: String)(run: E => F[A]): Proc[F, E, (E, A)] =
    alongside(Op(name, run))

  /**
   * The same, for a whole SUB-PROCEDURE rather than one operation:
   * run it on the environment and append its answer. An `if` inside a
   * block compiles to a sub-procedure, so this is the node that puts
   * the branch's value where the next statement can read it.
   */
  def alongside[F[+_], E, A](p: Proc[F, E, A]): Proc[F, E, (E, A)] =
    andThen(
      andThen(Arr((e: E) => (e, e)), First[F, E, A, E](p)),
      Arr((ae: (A, E)) => (ae._2, ae._1)))

  /** the choice, as a door: run the step on the right of a sum and
   * pass the left through */
  def onRight[F[+_], X, Y, C](f: Proc[F, X, Y]): Proc[F, Either[C, X], Either[C, Y]] =
    OnRight(f)

  /**
   * A STRAIGHT-LINE BLOCK, COMPILED TO AN ARROW
   * (specs/proc-notation.md).
   *
   * The block reads like the monadic workflow it mirrors — one `val`
   * per question, ordinary Scala between them — and what the macro
   * does is thread the ENVIRONMENT that a monadic body would keep in
   * its closure:
   *
   *     Proc.direct[Sig, Unit, String]: _ =>
   *       val city = !Question.Ask("city?")
   *       val t    = !Question.Now()
   *       s"\$city/\$t"
   *
   * Every `!` marks an OPERATION of the signature, never a `Proc`:
   * a step chosen by a value the block binds is `ArrowApply`, which
   * is a monad, and the macro refuses it by name. That refusal is the
   * whole difference between this and `direct` — see the spec.
   */
  inline def direct[F[+_], X, Y](inline block: ProcCtx[F] ?=> X => Y): Proc[F, X, Y] =
    ${ ProcMacro.impl[F, X, Y]('block) }

  /**
   * THE CAPABILITY, and it exists only INSIDE a `Proc.direct` block —
   * the same gate `Direct.DirectCtx` is, for the same reason. The
   * auto-colouring conversion below requires it, so outside a block
   * it cannot resolve and an operation used as a value stays the
   * compile error it always was. A compile-time refusal, where the
   * phantom marks can only throw at run time.
   */
  @scala.annotation.implicitNotFound(
    "no Proc.ProcCtx[${F}]: a question reads as its answer only INSIDE a Proc.direct block.\n" +
      "Wrap the code in Proc.direct { x => ... } — or use an explicit mark (!q / q.reflect),\n" +
      "which needs no capability.")
  final class ProcCtx[F[+_]] private[okay] ()

  /**
   * A QUESTION READS AS ITS ANSWER — the block's operations colour
   * themselves, so `val city = ask("city?")` needs no `!`.
   *
   * A phantom: the macro rewrites every one of these calls, and the
   * body here only runs if one escapes, which it then says loudly
   * rather than compiling to nothing.
   *
   * ONE CONVERSION, WHERE `Direct` HAS TWO, and the difference is the
   * shape of the two roads rather than an omission. A monadic block
   * distinguishes its OWN programs (`selfColor`) from an effect
   * signature's operations (`opColor`, gated by a `Direct.Effect`
   * marker), because a program of the row and an operation of the row
   * are different things there. A term's leaves are operations of one
   * signature and nothing else, so there is one case — and the
   * marker gate has nothing to add, because the capability already
   * names `F`: a type that is not this block's signature does not
   * match, whatever it declares about itself.
   */
  given procColor[F[+_], A](using ProcCtx[F]): Conversion[F[A], A] =
    _ => throw new IllegalStateException(
      "Proc auto-colouring escaped macro rewriting — this call belongs inside Proc.direct")

  /**
   * THE ALGEBRA, and it is one given with both halves, the way
   * `Mealy.mealyArrow` is: an arrow whose choice is available is an
   * ArrowChoice, and splitting them would make every call site
   * summon two.
   *
   * The laws are `okay.laws.ArrowLaws` — the shared suite, which this
   * instance instantiates in three lines rather than restating
   * (specs/arrows-plan.md, Decision 2).
   */
  given procArrow[F[+_]]: (Optic.Arrow[[X, Y] =>> Proc[F, X, Y]] & Optic.Choice[[X, Y] =>> Proc[F, X, Y]]) =
    new Optic.Arrow[[X, Y] =>> Proc[F, X, Y]] with Optic.Choice[[X, Y] =>> Proc[F, X, Y]]:
      def arr[A, B](f: A => B): Proc[F, A, B] = Arr(f)
      def compose[A, B, C](g: Proc[F, B, C], f: Proc[F, A, B]): Proc[F, A, C] = Then(f, g)
      def first[A, B, C](p: Proc[F, A, B]): Proc[F, (A, C), (B, C)] = First(p)
      def right[A, B, C](p: Proc[F, A, B]): Proc[F, Either[C, A], Either[C, B]] = OnRight(p)
      def dimap[A, B, C, D](p: Proc[F, A, B])(f: C => A, g: B => D): Proc[F, C, D] =
        Then(Then(Arr(f), p), Arr(g))

  /**
   * WHERE IN THE TERM. A position is a path because the term is a
   * tree — which is the sentence the whole design turns on, and the
   * reason `Free` cannot have one.
   */
  enum Step:
    // the two halves of a `Then` (one Scaladoc cannot cover two cases)
    case Fst
    case Snd
    /** inside a `First` or an `OnRight` */
    case In
    /** inside an `Iter`'s body, on its nth turn (from 0) */
    case Round(n: Int)
    /** inside a `Par`'s nth branch (0 left, 1 right) — its own step
     * and not `Fst`/`Snd`, so a path says which KIND of node it went
     * into and `render` knows to indent */
    case Side(n: Int)
    /** inside an `Undo`'s step (`back = false`) or its compensation
     * (`back = true`) */
    case Back(back: Boolean)

  opaque type Path = List[Step]

  object Path:
    val root: Path = Nil
    extension (p: Path)
      def /(s: Step): Path = p :+ s
      def steps: List[Step] = p
      def show: String = if p.isEmpty then "." else p.map:
        case Step.Fst => "1"
        case Step.Snd => "2"
        case Step.In => "in"
        case Step.Round(n) => s"round$n"
        case Step.Side(n) => s"par$n"
        case Step.Back(b) => if b then "undo" else "do"
      .mkString("/")

  /** a leaf of the term, where it is */
  final case class Leaf(name: String, at: Path)

  extension [F[+_], X, Y](p: Proc[F, X, Y])

    /**
     * EVERY LEAF THIS PROCEDURE MAY REACH, in term order, with its
     * path.
     *
     * An upper bound, and exactly the one `Static.leaves` reports for
     * the same reason: both sides of an `OnRight` are there because
     * which side runs is decided by a value that does not exist yet,
     * and an `Iter`'s body is counted ONCE because how often it runs
     * is decided the same way. An upper bound is what a capability
     * list, a picture and a deploy check all want.
     */
    def leaves: Vector[Leaf] =
      val out = Vector.newBuilder[Leaf]
      nodes(p, Path.root): (q, at) =>
        q match
          case Op(name, _) => out += Leaf(name, at)
          case _ => ()
      out.result()

    /**
     * THE TERM, INTERPRETED INTO ANY MONAD — `Static.foldMap`'s
     * counterpart, and the reason it asks for a `Monad` where the
     * applicative asks only for a `Selective`: an `Iter`'s trip count
     * is decided by a VALUE, and nothing weaker can run a step it has
     * to count.
     *
     * STACK: the `Iter` loop recurses through `G`'s own `flatMap`, so
     * it costs no host stack at a monad whose bind is lazy —
     * `Free.Bind` holds its continuation as a value, which is every
     * program row. At a STRICT monad the loop costs one frame per
     * turn, which is stated rather than discovered.
     */
    def foldMap[G[_]](nt: F ==> G)(using G: Monad[G]): X => G[Y] =
      p match
        case Arr(f) => x => G.pure(f(x))
        case Op(_, run) => x => nt(run(x))
        case Then(f, g) =>
          val fg = f.foldMap(nt)
          val gg = g.foldMap(nt)
          x => fg(x).flatMap(gg)
        case First(f) =>
          val ff = f.foldMap(nt)
          x => ff(x._1).map(b => (b, x._2))
        case OnRight(f) =>
          val ff = f.foldMap(nt)
          x => x match
            case Left(c) => G.pure(Left(c))
            case Right(a) => ff(a).map(Right(_))
        case Iter(body) =>
          val bb = body.foldMap(nt)
          def loop(x: X): G[Y] = bb(x).flatMap:
            case Left(again) => loop(again)
            case Right(y) => G.pure(y)
          loop(_)

        // LEFT THEN RIGHT, and the order is the contract rather than
        // an implementation detail: it is the order the journal
        // records the two branches' answers in, and the order
        // `Wf.Proc.walk` reports them in. A `Monad` cannot run two
        // things at once in any case — what a `Par` parallelises is
        // the waiting, which happens outside this fold entirely.
        case Par(a, b) =>
          val aa = a.foldMap(nt)
          val bb = b.foldMap(nt)
          x => aa(x).flatMap(y => bb(x).map(z => (y, z)))

        // THE STEP, AND ONLY THE STEP. Interpreting the inverse here
        // would run it on the way past, which is the opposite of what
        // it is for; a compensation is a term `Wf.Proc.compensating`
        // BUILDS from the nodes that completed, and then the engine
        // runs that like any other.
        case Undo(step, _) => step.foldMap(nt)

    /**
     * The same term as an ordinary program of the signature's row —
     * `Static.toFree`'s counterpart, and the bridge every runtime in
     * this library already knows how to drive.
     */
    def toProgram(x: X): Y ! F = p.foldMap[[Z] =>> Z ! F]([Z] => (fz: F[Z]) => Free.Inject(fz))(x)

    /**
     * THE TERM, DRAWN, with a position optionally marked.
     *
     * It is taken from the TERM and not from a status projection a
     * worker has to keep in step, which is the difference a static
     * spine buys: the picture cannot disagree with the program.
     */
    def render(at: Option[Path] = None): String =
      val b = StringBuilder()
      nodes(p, Path.root): (q, path) =>
        val mark = if at.contains(path) then "  <-- here" else ""
        // the indentation IS the path: every step that goes inside a
        // node (a `First`, an `OnRight`, an `Iter`) is one level, and
        // a `Then` is pure structure with nothing to draw
        val depth = path.steps.count:
          case Step.In => true
          case Step.Round(_) => true
          case Step.Side(_) => true
          case Step.Back(_) => true
          case _ => false
        val pad = "  " * depth
        q match
          case Arr(_) => b ++= s"$pad.$mark\n"
          case Op(name, _) => b ++= s"$pad$name$mark\n"
          case Then(_, _) => ()
          case First(_) => b ++= s"${pad}first$mark\n"
          case OnRight(_) => b ++= s"${pad}right$mark\n"
          case Iter(_) => b ++= s"${pad}loop$mark\n"
          case Par(_, _) => b ++= s"${pad}par$mark\n"
          case Undo(_, _) => b ++= s"${pad}undoable$mark\n"
      b.result()

    /**
     * THE TERM AS A PICTURE (specs/static-workflow.md stage 4), in
     * Mermaid, which this repository's docs already render.
     *
     * It is drawn FROM THE TERM, and that is the difference worth the
     * feature: a monadic engine can only draw a process from a status
     * projection somebody keeps in step with the code by hand, so the
     * picture and the program drift and the picture is the one nobody
     * checks. Here they cannot disagree — it is the same value the
     * engine runs.
     *
     * What it shows: every leaf by the name the AUTHOR gave its door,
     * BOTH sides of a choice (which one runs is decided by a value
     * that does not exist yet — the same upper bound `leaves`
     * reports), and a back edge for an `Iter`. Pure steps are not
     * drawn: an `Arr` performs nothing, and a picture of the plumbing
     * between two questions is a picture of nothing.
     *
     * `at` marks where a run stands — the path `Wf.Proc.walk` answers,
     * so a dashboard draws a position without replaying anything.
     */
    def mermaid(at: Option[Path] = None): String =
      val b = StringBuilder("flowchart TD\n")
      var n = 0
      def fresh(kind: String): String =
        n += 1
        s"$kind$n"
      def edge(from: String, to: String, label: String = ""): Unit =
        if label.isEmpty then b ++= s"  $from --> $to\n"
        else b ++= s"  $from -->|$label| $to\n"
      def mark(id: String, path: Path): Unit =
        if at.contains(path) then b ++= s"  class $id here\n"
      /** draw `q`, entered from `in`, and answer where it leaves */
      def go(q: Proc[F, ?, ?], path: Path, in: String): String = q match
        case Arr(_) => in
        case Op(name, _) =>
          val id = fresh("q")
          b ++= s"""  $id["$name"]\n"""
          mark(id, path)
          edge(in, id)
          id
        case Then(f, g) => go(g, path / Step.Snd, go(f, path / Step.Fst, in))
        case First(f) => go(f, path / Step.In, in)
        case OnRight(f) =>
          val choice = fresh("c")
          b ++= s"""  $choice{"which side?"}\n"""
          mark(choice, path)
          edge(in, choice)
          val taken = go(f, path / Step.In, choice)
          val join = fresh("j")
          b ++= s"  $join(( ))\n"
          edge(taken, join, "right")
          edge(choice, join, "left")
          join
        case Par(f, g) =>
          // A FORK AND A JOIN, both drawn, because a reader's first
          // question about a parallel branch is where it comes back
          // together — and the answer is a fact of the term rather
          // than of a runtime that might or might not be concurrent
          val fork = fresh("p")
          b ++= s"""  $fork{{"both"}}\n"""
          mark(fork, path)
          edge(in, fork)
          val l = go(f, path / Step.Side(0), fork)
          val r = go(g, path / Step.Side(1), fork)
          val join = fresh("j")
          b ++= s"  $join(( ))\n"
          edge(l, join)
          edge(r, join)
          join

        case Undo(step, undo) =>
          val out = go(step, path / Step.Back(false), in)
          // THE COMPENSATION IS DRAWN OFF THE PATH, on a dotted edge,
          // because that is the truth about it: it does not run on the
          // way past. A reader has to see both that it exists and that
          // it is not in the flow.
          val c = fresh("u")
          b ++= s"""  $c{"on failure"}\n"""
          b ++= s"  $out -.-> $c\n"
          val _ = go(undo, path / Step.Back(true), c)
          out

        case Iter(body) =>
          val head = fresh("l")
          b ++= s"""  $head{"loop"}\n"""
          mark(head, path)
          edge(in, head)
          // ONE round is drawn and the back edge says the rest: how
          // often the body runs is decided by a value that does not
          // exist yet, so a picture that unrolled it would be lying
          // about a number it cannot know
          val out = go(body, path / Step.Round(0), head)
          edge(out, head, "again")
          out
      b ++= "  s0(( ))\n"
      val last = go(p, Path.root, "s0")
      b ++= "  e0(( ))\n"
      edge(last, "e0")
      b ++= "  classDef here stroke-width:3px\n"
      b.result()

  /**
   * EVERY NODE OF THE TERM WITH ITS PATH, outermost first — and it
   * answers a question specs/arrows-plan.md Decision 4 asked rather
   * than predicted: are `leaves`, `walk` and `render` one
   * path-indexed fold?
   *
   * TWO OF THE THREE ARE. `leaves` keeps the `Op` nodes and `render`
   * draws all of them, each one line over this walk, with the
   * indentation read off the path. The third is NOT, and the reason
   * is structural rather than a matter of effort: `Wf.Proc.walk`
   * THREADS A VALUE and a journal through the term and may stop in
   * the middle, so it is an interpreter over two inputs, not a
   * traversal of one term. Written as a fold it would need the
   * accumulator to carry the value, the remaining journal, the count
   * and an early exit — which is the interpreter with a fold's
   * spelling on top.
   *
   * So the indexed-optics seat this was looking for is worth ONE
   * entry and not three: a fourth path-carrying walk exists here, and
   * it is this one.
   */
  private def nodes[F[+_]](q: Proc[F, ?, ?], at: Path)(f: (Proc[F, ?, ?], Path) => Unit): Unit =
    f(q, at)
    q match
      case Then(a, b) =>
        nodes(a, at / Step.Fst)(f)
        nodes(b, at / Step.Snd)(f)
      case First(a) => nodes(a, at / Step.In)(f)
      case OnRight(a) => nodes(a, at / Step.In)(f)
      case Iter(body) => nodes(body, at / Step.Round(0))(f)
      case Par(a, b) =>
        nodes(a, at / Step.Side(0))(f)
        nodes(b, at / Step.Side(1))(f)
      case Undo(a, b) =>
        nodes(a, at / Step.Back(false))(f)
        nodes(b, at / Step.Back(true))(f)
      case Arr(_) | Op(_, _) => ()
