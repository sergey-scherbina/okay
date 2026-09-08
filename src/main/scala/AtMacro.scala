package okay

import scala.quoted.*

/**
 * PROBE (rowlift): can a macro give `.at[R]` the ergonomics of a
 * postfix and the cost of constructing at the row directly?
 *
 * The measured problem: `.at` on a program rebuilds one Inject per
 * operation (+32 B/op against the floor, and HotSpot does not
 * scalarise it). But the receiver is almost always a LITERAL
 * construction — `State.get[Int]` is an inline def whose body is
 * `effect(Get())`, i.e. `Free.inject(Get())` — so at the call site
 * the macro sees the operation itself, and rebuilding is pointless:
 * emit the Inject at R in the first place.
 *
 * The general case still has to work, so the macro falls back to the
 * runtime walk when the receiver is anything else. Zero cost where
 * the shape is known, correct everywhere.
 *
 * Lives in src/main because a macro cannot be used in the compilation
 * run that defines it, and the JMH sources are a later one.
 */
object AtMacro:

  /** the membership witness: inj is identity at every instance, so
   * this is a type-level fact with no runtime content */
  trait In[F[+_], R[+_]]:
    def inj[A](fa: F[A]): R[A]

  private object IdIn extends In[[A] =>> Any, [A] =>> Any]:
    def inj[A](fa: Any): Any = fa

  trait InLow:
    given self[F[+_]]: In[F, F] = IdIn.asInstanceOf[In[F, F]]

  object In extends InLow:
    given left[F[+_], G[+_]]: In[F, F + G] = IdIn.asInstanceOf[In[F, F + G]]
    given deeper[F[+_], G[+_], H[+_]](using i: In[F, G]): In[F, G + H] =
      IdIn.asInstanceOf[In[F, G + H]]

  /** the fallback: the tree walk, for a receiver the macro cannot see
   * through. Not inline — it recurses, and inline recursion does not
   * terminate. */
  def walk[A, F[+_], R[+_]](p: A ! F)(i: In[F, R]): A ! R =
    import okay.!.*
    (p.resume: @unchecked) match
      case Pure(a) => Free.Pure(a)
      case Effect(e) => Free.inject(i.inj(e))
      case Bind(Effect(e), k) => Free.inject(i.inj(e)).flatMap(x => walk(k(x))(i))

  extension [A, F[+_]](p: A ! F)
    inline def at[R[+_]](using inline i: In[F, R]): A ! R =
      ${ atImpl[A, F, R]('p, 'i) }

  /**
   * Scala 3 binds an extension's receiver to a val proxy BEFORE the
   * splice, so a naive stripper sees `Ident("p$proxy1")` and nothing
   * else — which is exactly why the first version of this macro fell
   * back every time and measured identical to widen. The bindings of
   * the enclosing Inlined carry the real term; collect them on the
   * way down and resolve the identifier against them.
   */
  private def resolved(using q: Quotes)
                      (t: q.reflect.Term,
                       env: Map[q.reflect.Symbol, q.reflect.Term])
  : q.reflect.Term =
    import q.reflect.*
    def binds(ss: List[Statement]): Map[Symbol, Term] =
      ss.collect { case vd: ValDef if vd.rhs.isDefined => vd.symbol -> vd.rhs.get }.toMap
    t match
      case Inlined(_, bs, inner) => resolved(inner, env ++ binds(bs))
      case Block(ss, inner) => resolved(inner, env ++ binds(ss))
      case Typed(inner, _) => resolved(inner, env)
      case id: Ident if env.contains(id.symbol) => resolved(env(id.symbol), env)
      case _ => t

  private def isInject(using q: Quotes)(fn: q.reflect.Term): Boolean =
    val n = fn.symbol.name
    n == "inject" || n == "effect" ||
      (n == "apply" && fn.symbol.owner.name.startsWith("Inject"))

  def atImpl[A: Type, F[+_]: Type, R[+_]: Type](p: Expr[A ! F], i: Expr[In[F, R]])
                                               (using Quotes): Expr[A ! R] =
    import quotes.reflect.*
    resolved(p.asTerm, Map.empty) match
      // Free.inject[F, A](op) / effect[F, A](op) — the operation is
      // right there, so put it in R's Inject and skip the rebuild
      case Apply(fn, List(op)) if isInject(fn) =>
        op.asExpr match
          case '{ $o: t } =>
            '{ Free.inject[R, A]($i.inj(${ op.asExprOf[F[A]] })) }
      case _ =>
        '{ walk[A, F, R]($p)($i) }
