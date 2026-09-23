package okay.clojure

import clojure.java.api.Clojure
import clojure.lang.{IFn, Namespace, RT, Symbol, Var}

/**
 * Calling Clojure from okay, through Clojure's own Java API
 * (`clojure.java.api.Clojure`) and nothing else: a var is an `IFn`,
 * and `invoke` is the call.
 *
 * What this adds is the refusal. `Clojure.var` answers a Var for ANY
 * name, bound or not, and the mistake surfaces later as an
 * `IllegalStateException: Attempting to call unbound fn` from deep in
 * a call — so a var that is not there, or a namespace that does not
 * load, is a `Left` naming it here instead.
 */
object Clj {

  private lazy val requireFn: IFn = Clojure.`var`("clojure.core", "require")
  private lazy val loadString: IFn = Clojure.`var`("clojure.core", "load-string")

  /**
   * load a namespace (idempotent, as `require` is) — unless it already
   * EXISTS: a namespace made at run time (by `eval`, `in-ns`, a REPL) has
   * no file for `require` to find, and asking for it failed with "Could
   * not locate user__init.class" (found by clojure-core-async's go-block
   * test, whose `defn` lives in `user`)
   */
  def require(ns: String): Either[String, Unit] =
    if Namespace.find(Symbol.intern(ns)) != null then Right(())
    else
      try { requireFn.invoke(Clojure.read(ns)): Unit; Right(()) }
      catch case e: Exception => Left(s"cannot load namespace $ns: ${e.getMessage}")

  /** the function a var holds, its namespace loaded first */
  def fn(ns: String, name: String): Either[String, IFn] =
    require(ns).flatMap { _ =>
      Clojure.`var`(ns, name) match
        case v: Var if v.isBound => Right(v)
        case _ => Left(s"no bound var $ns/$name")
    }

  /** the value a var holds (a `def`), its namespace loaded first */
  def value(ns: String, name: String): Either[String, AnyRef] =
    require(ns).flatMap { _ =>
      Clojure.`var`(ns, name) match
        case v: Var if v.isBound => Right(v.deref())
        case _ => Left(s"no bound var $ns/$name")
    }

  // `refer-clojure` is a macro; `refer` is the function it expands to
  private lazy val refer: IFn = Clojure.`var`("clojure.core", "refer")

  /**
   * Read and evaluate Clojure source IN a namespace (default `user`),
   * created — and given clojure.core, as `ns` would — if it is new; the
   * last form's value. Without the binding, code evaluated from Java runs
   * in whatever `*ns*` is, which is `clojure.core`: a `defn` evaluated
   * here defined its function INSIDE clojure.core (found by
   * clojure-core-async's go-block test).
   */
  def eval(source: String, ns: String = "user"): Either[String, AnyRef] =
    val sym = Symbol.intern(ns)
    val fresh = Namespace.find(sym) == null
    val space = Namespace.findOrCreate(sym)
    Var.pushThreadBindings(RT.map(RT.CURRENT_NS, space))
    try
      if fresh then refer.invoke(Symbol.intern("clojure.core")): Unit
      Right(loadString.invoke(source))
    catch case e: Exception => Left(s"clojure: ${e.getMessage}")
    finally Var.popThreadBindings()
}
