package okay.conf

import scala.scalajs.js

/** the JS half: Node's process.env is the environment; files wait
 * for a consumer that needs Node's fs (the browser has no secrets
 * to resolve and should not) */
private[conf] object Platform {

  val env: Secrets = s =>
    Secrets.scheme(s.ref) match
      case ("env", name) =>
        val v = js.Dynamic.global.process.env.selectDynamic(name)
        if js.isUndefined(v) then Left(s"'env:$name' is not set")
        else Right(v.toString)
      case _ => Secrets.unrecognized(s)

  val file: Secrets = s =>
    Secrets.scheme(s.ref) match
      case ("file", _) => Left("'file:' is a JVM/Native ability (Node fs joins when a consumer needs it)")
      case _ => Secrets.unrecognized(s)

  private[conf] def slurp(@scala.annotation.unused path: String): Either[String, String] =
    Left("Conf.load is a JVM/Native ability (Node fs joins when a consumer needs it)")
}
