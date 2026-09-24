package okay2

import okay2.Optic.{Arrow, Choice}

/**
 * A PROCEDURE WHOSE EVERY STEP IS KNOWN BEFORE IT RUNS — the free arrow
 * over a signature, the Scala 3 core's okay-workflow `Proc`. `Static`
 * is the free applicative; this is its arrow, where a step may look at
 * what came before it. What is deliberately absent is `ArrowApply`
 * (Hughes 2000: that is a monad): no operation takes a computation as
 * data, so the term is finite and walkable — "where does this run
 * stand" is a PATH, "which operations may it reach" is answerable
 * before it starts. `Iter` is Elgot iteration as a node.
 *
 * Over an okay2 ROW, where the core's is a type constructor `F[+_]`: a
 * leaf builds an `F#Op[Y]`. The interpreters are METHODS of the nodes
 * (`foldMap`, `walk`), where each node's own types are known — scalac 2
 * does not refine a method's type parameters from a constructor
 * pattern, and a match over the nodes would need a cast per arm.
 */
sealed abstract class Proc[F <: Row, X, Y] {
  import Proc._

  /** the term interpreted into any monad; an `Iter` recurses through
   * `G`'s own `flatMap`, so a lazy bind (every program row) costs no
   * host stack */
  def foldMap[G[_]](nt: Static.To[F, G])(implicit G: Monad[G]): X => G[Y]

  /** the fold that threads a value and a state and may stop — the
   * shape `Wf.Proc.walk` needs, with the leaves and the three nodes
   * whose meaning is the walker's (`Op`, `Undo`, `Par`) left to it */
  private[okay2] def walk[S, B](x: X, s: S, at: Path, w: Walker[F, S, B]): Walked[S, B, Y]

  /** the direct children, with the step into each */
  private[okay2] def children: List[(Proc[F, _, _], Step)]

  /** the same term as an ordinary program of the signature's row */
  def toProgram(x: X): Y ! F = {
    type L[Z] = Free[F, Z]
    foldMap[L](new Static.To[F, L] { def apply[Z](op: F#Op[Z]): Free[F, Z] = Free.inject[F, Z](op) })(Free.monad[F]).apply(x)
  }

  /** every leaf this procedure MAY reach, in term order, with its path
   * — an upper bound: both sides of a choice, an `Iter`'s body once */
  def leaves: Vector[Leaf] = {
    val out = Vector.newBuilder[Leaf]
    nodes(this, Path.root) { (q, at) =>
      q match {
        case o: Op[F, _, _] => out += Leaf(o.name, at)
        case _ => ()
      }
    }
    out.result()
  }

  /** the term, drawn, with a position optionally marked */
  def render(at: Option[Path] = None): String = {
    val b = new StringBuilder
    nodes(this, Path.root) { (q, path) =>
      val mark = if (at.contains(path)) "  <-- here" else ""
      val depth = path.steps.count {
        case Step.In | Step.Round(_) | Step.Side(_) | Step.Back(_) => true
        case _ => false
      }
      val pad = "  " * depth
      q match {
        case _: Arr[F, _, _] => b ++= s"$pad.$mark\n"
        case o: Op[F, _, _] => b ++= s"$pad${o.name}$mark\n"
        case _: Then[F, _, _, _] => ()
        case _: First[F, _, _, _] => b ++= s"${pad}first$mark\n"
        case _: OnRight[F, _, _, _] => b ++= s"${pad}right$mark\n"
        case _: Iter[F, _, _] => b ++= s"${pad}loop$mark\n"
        case _: Par[F, _, _, _] => b ++= s"${pad}par$mark\n"
        case _: Undo[F, _, _] => b ++= s"${pad}undoable$mark\n"
      }
    }
    b.result()
  }

  /** the term as a Mermaid picture, drawn FROM THE TERM — it cannot
   * disagree with the program; `at` marks where a run stands */
  def mermaid(at: Option[Path] = None): String = {
    val b = new StringBuilder("flowchart TD\n")
    var n = 0
    def fresh(kind: String): String = { n += 1; s"$kind$n" }
    def edge(from: String, to: String, label: String = ""): Unit =
      if (label.isEmpty) b ++= s"  $from --> $to\n" else b ++= s"  $from -->|$label| $to\n"
    def mark(id: String, path: Path): Unit = if (at.contains(path)) b ++= s"  class $id here\n"
    def go(q: Proc[F, _, _], path: Path, in: String): String = q match {
      case _: Arr[F, _, _] => in
      case o: Op[F, _, _] =>
        val id = fresh("q")
        b ++= s"""  $id["${o.name}"]\n"""
        mark(id, path)
        edge(in, id)
        id
      case t: Then[F, _, _, _] => go(t.g, path / Step.Snd, go(t.f, path / Step.Fst, in))
      case f: First[F, _, _, _] => go(f.f, path / Step.In, in)
      case r: OnRight[F, _, _, _] =>
        val choice = fresh("c")
        b ++= s"""  $choice{"which side?"}\n"""
        mark(choice, path)
        edge(in, choice)
        val taken = go(r.f, path / Step.In, choice)
        val join = fresh("j")
        b ++= s"  $join(( ))\n"
        edge(taken, join, "right")
        edge(choice, join, "left")
        join
      case p: Par[F, _, _, _] =>
        val fork = fresh("p")
        b ++= s"""  $fork{{"both"}}\n"""
        mark(fork, path)
        edge(in, fork)
        val l = go(p.f, path / Step.Side(0), fork)
        val r = go(p.g, path / Step.Side(1), fork)
        val join = fresh("j")
        b ++= s"  $join(( ))\n"
        edge(l, join)
        edge(r, join)
        join
      case u: Undo[F, _, _] =>
        val out = go(u.step, path / Step.Back(false), in)
        // off the path, on a dotted edge: it does not run on the way past
        val c = fresh("u")
        b ++= s"""  $c{"on failure"}\n"""
        b ++= s"  $out -.-> $c\n"
        val _ = go(u.undo, path / Step.Back(true), c)
        out
      case it: Iter[F, _, _] =>
        val head = fresh("l")
        b ++= s"""  $head{"loop"}\n"""
        mark(head, path)
        edge(in, head)
        // ONE round is drawn and the back edge says the rest
        val out = go(it.body, path / Step.Round(0), head)
        edge(out, head, "again")
        out
    }
    b ++= "  s0(( ))\n"
    val last = go(this, Path.root, "s0")
    b ++= "  e0(( ))\n"
    edge(last, "e0")
    b ++= "  classDef here stroke-width:3px\n"
    b.result()
  }
}

object Proc {

  /** a pure step: no operation, nothing journalled */
  final case class Arr[F <: Row, X, Y](f: X => Y) extends Proc[F, X, Y] {
    def foldMap[G[_]](nt: Static.To[F, G])(implicit G: Monad[G]): X => G[Y] = x => G.pure(f(x))
    private[okay2] def walk[S, B](x: X, s: S, at: Path, w: Walker[F, S, B]): Walked[S, B, Y] = Ran(f(x), s)
    private[okay2] def children: List[(Proc[F, _, _], Step)] = Nil
  }

  /** THE LEAF — one operation of the signature, built from the input,
   * and a NAME: the static content a reader, a picture and a deploy
   * check can have before the input arrives */
  final case class Op[F <: Row, X, Y](name: String, run: X => F#Op[Y]) extends Proc[F, X, Y] {
    def foldMap[G[_]](nt: Static.To[F, G])(implicit G: Monad[G]): X => G[Y] = x => nt(run(x))
    private[okay2] def walk[S, B](x: X, s: S, at: Path, w: Walker[F, S, B]): Walked[S, B, Y] = w.op(run(x), s, at)
    private[okay2] def children: List[(Proc[F, _, _], Step)] = Nil
  }

  final case class Then[F <: Row, X, Y, Z](f: Proc[F, X, Y], g: Proc[F, Y, Z]) extends Proc[F, X, Z] {
    def foldMap[G[_]](nt: Static.To[F, G])(implicit G: Monad[G]): X => G[Z] = {
      val fg = f.foldMap(nt); val gg = g.foldMap(nt)
      x => G.flatMap(fg(x))(gg)
    }
    private[okay2] def walk[S, B](x: X, s: S, at: Path, w: Walker[F, S, B]): Walked[S, B, Z] =
      f.walk(x, s, at / Step.Fst, w) match {
        case Ran(v, s2) => g.walk(v, s2, at / Step.Snd, w)
        case st: Stopped[S, B] => st
      }
    private[okay2] def children: List[(Proc[F, _, _], Step)] = List((f, Step.Fst), (g, Step.Snd))
  }

  /** the strength: carry a value past a step untouched */
  final case class First[F <: Row, X, Y, C](f: Proc[F, X, Y]) extends Proc[F, (X, C), (Y, C)] {
    def foldMap[G[_]](nt: Static.To[F, G])(implicit G: Monad[G]): ((X, C)) => G[(Y, C)] = {
      val ff = f.foldMap(nt)
      x => G.fmap(ff(x._1), (b: Y) => (b, x._2))
    }
    private[okay2] def walk[S, B](x: (X, C), s: S, at: Path, w: Walker[F, S, B]): Walked[S, B, (Y, C)] =
      f.walk(x._1, s, at / Step.In, w) match {
        case Ran(v, s2) => Ran((v, x._2), s2)
        case st: Stopped[S, B] => st
      }
    private[okay2] def children: List[(Proc[F, _, _], Step)] = List((f, Step.In))
  }

  /** the choice: run the step on the right of a sum, pass the left through */
  final case class OnRight[F <: Row, X, Y, C](f: Proc[F, X, Y]) extends Proc[F, Either[C, X], Either[C, Y]] {
    def foldMap[G[_]](nt: Static.To[F, G])(implicit G: Monad[G]): Either[C, X] => G[Either[C, Y]] = {
      val ff = f.foldMap(nt)
      (x: Either[C, X]) => x match {
        case Left(c) => G.pure(Left(c): Either[C, Y])
        case Right(a) => G.fmap(ff(a), (y: Y) => Right(y): Either[C, Y])
      }
    }
    private[okay2] def walk[S, B](x: Either[C, X], s: S, at: Path, w: Walker[F, S, B]): Walked[S, B, Either[C, Y]] = x match {
      case Left(c) => Ran(Left(c), s)
      case Right(a) => f.walk(a, s, at / Step.In, w) match {
        case Ran(v, s2) => Ran(Right(v), s2)
        case st: Stopped[S, B] => st
      }
    }
    private[okay2] def children: List[(Proc[F, _, _], Step)] = List((f, Step.In))
  }

  /** Elgot iteration: `Left` goes round from the new value, `Right` leaves */
  final case class Iter[F <: Row, X, Y](body: Proc[F, X, Either[X, Y]]) extends Proc[F, X, Y] {
    def foldMap[G[_]](nt: Static.To[F, G])(implicit G: Monad[G]): X => G[Y] = {
      val bb = body.foldMap(nt)
      def loop(x: X): G[Y] = G.flatMap(bb(x)) {
        case Left(again) => loop(again)
        case Right(y) => G.pure(y)
      }
      loop(_)
    }
    private[okay2] def walk[S, B](x: X, s: S, at: Path, w: Walker[F, S, B]): Walked[S, B, Y] = {
      @scala.annotation.tailrec def loop(cur: X, st: S, round: Int): Walked[S, B, Y] =
        body.walk(cur, st, at / Step.Round(round), w) match {
          case Ran(Left(again), s2) => loop(again, s2, round + 1)
          case Ran(Right(y), s2) => Ran(y, s2)
          case stop: Stopped[S, B] => stop
        }
      loop(x, s, 0)
    }
    private[okay2] def children: List[(Proc[F, _, _], Step)] = List((body, Step.Round(0)))
  }

  /** TWO INDEPENDENT BRANCHES OVER THE SAME INPUT: what is parallel is
   * the WAITING — both pending questions are known at once; answers
   * are still recorded left, then right */
  final case class Par[F <: Row, X, Y, Z](f: Proc[F, X, Y], g: Proc[F, X, Z]) extends Proc[F, X, (Y, Z)] {
    def foldMap[G[_]](nt: Static.To[F, G])(implicit G: Monad[G]): X => G[(Y, Z)] = {
      val ff = f.foldMap(nt); val gg = g.foldMap(nt)
      x => G.flatMap(ff(x))(y => G.fmap(gg(x), (z: Z) => (y, z)))
    }
    private[okay2] def walk[S, B](x: X, s: S, at: Path, w: Walker[F, S, B]): Walked[S, B, (Y, Z)] =
      f.walk(x, s, at / Step.Side(0), w) match {
        case Ran(y, s2) => g.walk(x, s2, at / Step.Side(1), w) match {
          case Ran(z, s3) => Ran((y, z), s3)
          case st: Stopped[S, B] => st
        }
        case Stopped(left) => Stopped(w.parStopped(left, g, x, at / Step.Side(1)))
      }
    private[okay2] def children: List[(Proc[F, _, _], Step)] = List((f, Step.Side(0)), (g, Step.Side(1)))
  }

  /** A STEP WITH ITS INVERSE BESIDE IT, given what the step was given
   * and what it produced. Under `foldMap` an `Undo` IS its step; the
   * compensations are FOUND by a walk, never carried on the edge */
  final case class Undo[F <: Row, X, Y](step: Proc[F, X, Y], undo: Proc[F, (X, Y), Unit]) extends Proc[F, X, Y] {
    def foldMap[G[_]](nt: Static.To[F, G])(implicit G: Monad[G]): X => G[Y] = step.foldMap(nt)
    private[okay2] def walk[S, B](x: X, s: S, at: Path, w: Walker[F, S, B]): Walked[S, B, Y] =
      step.walk(x, s, at / Step.Back(false), w) match {
        case Ran(y, s2) =>
          // built here, where x and y are this node's own types
          w.completed(andThen(Arr[F, Unit, (X, Y)](_ => (x, y)), undo))
          Ran(y, s2)
        case st: Stopped[S, B] => st
      }
    private[okay2] def children: List[(Proc[F, _, _], Step)] = List((step, Step.Back(false)), (undo, Step.Back(true)))
  }

  /** where a walk stopped, or the value and state it carries on with */
  sealed trait Walked[S, +B, +V]
  final case class Ran[S, V](value: V, s: S) extends Walked[S, Nothing, V]
  final case class Stopped[S, B](b: B) extends Walked[S, B, Nothing]

  /** the three decisions a walk leaves to its caller */
  trait Walker[F <: Row, S, B] {
    /** one leaf's question, answered from the state or not */
    def op[Y](q: F#Op[Y], s: S, at: Path): Walked[S, B, Y]
    /** an `Undo` whose step completed: its compensation, fed */
    def completed(undo: Proc[F, Unit, Unit]): Unit
    /** a `Par` whose LEFT branch stopped: what the stop becomes, given
     * the right branch it may walk speculatively */
    def parStopped[X, Z](left: B, right: Proc[F, X, Z], x: X, at: Path): B
  }

  /** one operation as a step — the door in */
  def op[F <: Row, X, Y](name: String)(run: X => F#Op[Y]): Proc[F, X, Y] = Op(name, run)

  /** a pure step */
  def arr[F <: Row, X, Y](f: X => Y): Proc[F, X, Y] = Arr(f)

  /** iteration, as the door */
  def iter[F <: Row, X, Y](body: Proc[F, X, Either[X, Y]]): Proc[F, X, Y] = Iter(body)

  /** two independent branches over the same input */
  def par[F <: Row, X, Y, Z](f: Proc[F, X, Y], g: Proc[F, X, Z]): Proc[F, X, (Y, Z)] = Par(f, g)

  /** a step with its compensation */
  def undoable[F <: Row, X, Y](step: Proc[F, X, Y])(undo: Proc[F, (X, Y), Unit]): Proc[F, X, Y] = Undo(step, undo)

  /** composition, with the one rewrite that pays for itself: two pure
   * steps in a row are one pure step */
  def andThen[F <: Row, X, Y, Z](f: Proc[F, X, Y], g: Proc[F, Y, Z]): Proc[F, X, Z] = (f, g) match {
    case (Arr(a), Arr(b)) => Arr(a.andThen(b))
    case _ => Then(f, g)
  }

  /** run an operation built from the environment and APPEND its answer
   * — a block's environment is a left-nested tuple growing by one */
  def keeping[F <: Row, E, A](name: String)(run: E => F#Op[A]): Proc[F, E, (E, A)] = alongside(Op[F, E, A](name, run))

  /** the same for a whole sub-procedure */
  def alongside[F <: Row, E, A](p: Proc[F, E, A]): Proc[F, E, (E, A)] =
    andThen(andThen(Arr[F, E, (E, E)](e => (e, e)), First[F, E, A, E](p)), Arr[F, (A, E), (E, A)](ae => (ae._2, ae._1)))

  /** the choice, as a door */
  def onRight[F <: Row, X, Y, C](f: Proc[F, X, Y]): Proc[F, Either[C, X], Either[C, Y]] = OnRight(f)

  /** the carrier at a fixed signature, as a two-hole type */
  type Of[F <: Row] = { type L[X, Y] = Proc[F, X, Y] }

  /** THE ALGEBRA: one instance with both halves, as `Mealy`'s is — an
   * arrow whose choice is available */
  implicit def procArrow[F <: Row]: Arrow[Of[F]#L] with Choice[Of[F]#L] = new Arrow[Of[F]#L] with Choice[Of[F]#L] {
    def arr[A, B](f: A => B): Proc[F, A, B] = Arr(f)
    def compose[A, B, C](g: Proc[F, B, C], f: Proc[F, A, B]): Proc[F, A, C] = Then(f, g)
    def first[A, B, C](p: Proc[F, A, B]): Proc[F, (A, C), (B, C)] = First(p)
    def right[A, B, C](p: Proc[F, A, B]): Proc[F, Either[C, A], Either[C, B]] = OnRight(p)
    def dimap[A, B, C, D](p: Proc[F, A, B])(f: C => A, g: B => D): Proc[F, C, D] = Then(Then(Arr(f), p), Arr(g))
  }

  /** WHERE IN THE TERM: a position is a path because the term is a tree */
  sealed trait Step
  object Step {
    case object Fst extends Step
    case object Snd extends Step
    /** inside a `First` or an `OnRight` */
    case object In extends Step
    /** inside an `Iter`'s body, on its nth turn (from 0) */
    final case class Round(n: Int) extends Step
    /** inside a `Par`'s nth branch (0 left, 1 right) */
    final case class Side(n: Int) extends Step
    /** inside an `Undo`'s step (`back = false`) or its compensation */
    final case class Back(back: Boolean) extends Step
  }

  /** the core's opaque `List[Step]`, as a value class */
  final class Path private (val steps: List[Step]) extends AnyVal {
    def /(s: Step): Path = new Path(steps :+ s)
    def show: String = if (steps.isEmpty) "." else steps.map {
      case Step.Fst => "1"
      case Step.Snd => "2"
      case Step.In => "in"
      case Step.Round(n) => s"round$n"
      case Step.Side(n) => s"par$n"
      case Step.Back(b) => if (b) "undo" else "do"
    }.mkString("/")
    override def toString: String = s"Path($show)"
  }

  object Path {
    val root: Path = new Path(Nil)
  }

  /** a leaf of the term, where it is */
  final case class Leaf(name: String, at: Path)

  /** every node with its path, outermost first */
  private def nodes[F <: Row](q: Proc[F, _, _], at: Path)(f: (Proc[F, _, _], Path) => Unit): Unit = {
    f(q, at)
    q.children.foreach { case (c, step) => nodes(c, at / step)(f) }
  }
}
