package okay.r

import okay.py.{CrashConformance, ForeignWorker, Shape, WireConformance, WireLink, WorkerCommand}

/** the conformance programs, in R: what Python, TypeScript, Go, Rust and
 * Haskell each serve for the same suites (foreign-one-value) */
object RConformance:
  val conf = R.module("conf", """
    pairs <- function() {
      okay_then(okay_perform("choose", c(1L, 2L)), function(x)
        okay_then(okay_perform("choose", c(10L, 20L)), function(y)
          okay_done(x + y)))
    }
    total <- function(sku, qty)
      okay_then(okay_perform("price_of", sku), function(price)
        okay_perform("discount", price * qty))
    boom <- function() stop("R says no")
    # DIRECT STYLE: ordinary R calling okay's effects
    quote <- function(sku, qty) {
      price <- okay_call("price_of", sku)
      okay_call("discount", price * qty)
    }
  """)

/** R is one more row of the ONE conformance body every wire language
 * answers to: multi-shot, callbacks under the caller's Reader, direct
 * style, a failure by name with R living on */
class TestRConformance extends WireConformance:
  override def munitIgnore: Boolean = TestR.rscript.isEmpty
  override def shape: Shape = R.shape
  override def address(name: String): String = s"conf::$name"
  lazy val engine: ForeignWorker = RSubprocess.worker(TestR.rscript.get, Seq(RConformance.conf))
  override def afterAll(): Unit = if !munitIgnore then engine.close()

/** R killed from outside — between two choices, while idle, mid-`okay_call` —
 * and recovered by the one supervisor: the crash suite every language runs */
class TestRCrash extends CrashConformance:
  override def munitIgnore: Boolean = TestR.rscript.isEmpty
  override def shape: Shape = R.shape
  override def address(name: String): String = s"conf::$name"
  def worker: WorkerCommand = RSubprocess.command(TestR.rscript.get, Seq(RConformance.conf))
  override def speaking(link: WireLink, name: String): ForeignWorker = RSubprocess.speaking(link, name)
