package okay.refine

import scala.quoted.*

/** a type as it is written, without its packages (`Swap | Cds`), for the
 * name of a route — `ClassTag` cannot give it: a union's class is its
 * least upper bound */
object TypeName:
  inline def of[X]: String = ${ ofImpl[X] }

  def ofImpl[X: Type](using Quotes): Expr[String] =
    Expr(Type.show[X].replaceAll("""(?:[\w$]+\.)+""", "").replace("$", ""))
