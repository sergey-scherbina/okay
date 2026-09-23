package okay.clojure

import clojure.java.api.Clojure
import clojure.lang.{IFn, Var}

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

  /** load a namespace (idempotent, as `require` is) */
  def require(ns: String): Either[String, Unit] =
    try { requireFn.invoke(Clojure.read(ns)): Unit; Right(()) }
    catch case e: Exception => Left(s"cannot load namespace $ns: ${e.getMessage}")

  /** the function a var holds, its namespace loaded first */
  def fn(ns: String, name: String): Either[String, IFn] =
    require(ns).flatMap { _ =>
      Clojure.`var`(ns, name) match
        case v: Var if v.isBound => Right(v)
        case _ => Left(s"no bound var $ns/$name")
    }

  /** read and evaluate Clojure source; the last form's value */
  def eval(source: String): Either[String, AnyRef] =
    try Right(loadString.invoke(source))
    catch case e: Exception => Left(s"clojure: ${e.getMessage}")
}
