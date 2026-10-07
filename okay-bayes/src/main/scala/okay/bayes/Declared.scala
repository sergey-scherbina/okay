package okay.bayes

import okay.freer.{!, Static}
/**
 * A MODEL WHOSE STRUCTURE IS KNOWN BEFORE IT RUNS (specs/okay-bayes.md
 * stage 8). The parameters are a `Static[Model, P]` — the core's free
 * Selective, so every `sample` they may perform is data before anything is
 * drawn — and the likelihood is a pure `P => Double`. An operation's
 * argument in a Static program is fixed when the program is built, which
 * is the price: a prior cannot depend on another draw (write a hierarchy
 * non-centered, θ = μ + τ·z) and the likelihood is a function, not an
 * operation.
 */
object Declared:
  type Params[A] = Static[Model, A]

  /** draw `name` from `d` — a leaf, visible before running */
  def sample[A](name: String, d: Distribution[A]): Params[A] = Static.op(Model.Sample(name, d))

  def pure[A](a: A): Params[A] = Static.Pure(a)

  /** two declarations side by side: neither depends on the other's value */
  def both[A, B](a: Params[A], b: Params[B]): Params[(A, B)] =
    Static.Ap(Static.Ap(Static.Pure((x: A) => (y: B) => (x, y)), a), b)

  /** every declaration, in order */
  def all[A](ps: Vector[Params[A]]): Params[Vector[A]] =
    ps.foldLeft(pure(Vector.empty[A]))((acc, p) => Static.Ap(Static.Ap(Static.Pure((xs: Vector[A]) => (x: A) => xs :+ x), acc), p))

  /** `n` draws `name[i]` from `d` */
  def sampleN[A](name: String, d: Distribution[A], n: Int): Params[Vector[A]] =
    all(Vector.tabulate(n)(i => sample(s"$name[$i]", d)))

  /** one site of a declaration: its name, its distribution, and whether it sits under a branch (it may not be drawn) */
  final case class Site(name: String, dist: Distribution[?], conditional: Boolean)

  /**
   * EVERY SITE the parameters may draw, with nothing run — the sites of
   * both sides of each `select`, marked conditional. An explicit stack, as
   * `Static.leaves`: a declaration built by `all` nests as deep as it is
   * long. A name declared twice outside any branch is refused here.
   */
  def sites(p: Params[?]): Vector[Site] =
    val out = Vector.newBuilder[Site]
    var todo: List[(Static[Model, ?], Boolean)] = (p, false) :: Nil
    while todo.nonEmpty do
      val (head, conditional) = todo.head
      todo = todo.tail
      head match
        case Static.Pure(_) => ()
        case Static.Op(Model.Sample(name, d)) => out += Site(name, d, conditional)
        case Static.Op(Model.Factor(_)) => ()
        case Static.Ap(f, a) => todo = (f, conditional) :: (a, conditional) :: todo
        case Static.Select(e, f) => todo = (e, conditional) :: (f, true) :: todo
    val found = out.result()
    found.filterNot(_.conditional).groupBy(_.name).collectFirst { case (n, v) if v.length > 1 => n }
      .foreach(n => throw IllegalArgumentException(s"Declared: site '$n' is declared twice"))
    found

  /** the declaration as an ordinary model — the parameters run, then weighed by the likelihood — for every sampler */
  def model[A](params: Params[A])(logLik: A => Double): A ! Model =
    val _ = sites(params)
    params.toFree.flatMap(a => Bayes.factor(logLik(a)).map(_ => a))

  /**
   * NUTS, with the structure checked BEFORE sampling: every site must be
   * continuous and none under a branch (a branch decides by a draw whether
   * a site exists, which a gradient cannot follow) — refused by name.
   */
  def nuts[A](params: Params[A], samples: Int, burn: Int = 1000, chains: Int = 1, seed: Long = 42L, delta: Double = 0.8)
    (logLik: A => Double)(using sampler: Sampler): Posterior[A] =
    val ss = sites(params)
    ss.find(_.conditional).foreach(s =>
      throw IllegalArgumentException(s"Declared.nuts: site '${s.name}' is under a branch — whether it is drawn depends on a draw; use metropolis"))
    ss.find(!_.dist.support.continuous).foreach(s =>
      throw IllegalArgumentException(s"Declared.nuts: site '${s.name}' is discrete (${s.dist}) — use metropolis or a kernel"))
    Bayes.nuts(model(params)(logLik), samples, burn, chains, seed, delta)
