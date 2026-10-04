package okay2.persist

import okay2.{!, Pure}
import okay2.async.Async
import okay2.platform._

/** the wire suites' runner: an Async program executed on the platform,
 * and the Live tag — every one of them binds a real port */
trait WireRun extends munit.FunSuite {
  def run[A](prog: A ! Async): A = !.run(Async.run[A, Pure](prog))

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))

  def bytes(s: String): Array[Byte] = s.getBytes("UTF-8")
  def str(b: Array[Byte]): String = new String(b, "UTF-8")
}
