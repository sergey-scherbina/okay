package okay.durable

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobalScope

@js.native
private trait WebCrypto extends js.Object:
  def randomUUID(): String = js.native

@js.native
@JSGlobalScope
private object RunGlobals extends js.Object:
  val crypto: WebCrypto = js.native

private[durable] object RunId:
  def fresh(): String =
    if js.typeOf(RunGlobals.crypto) == "undefined" then
      throw IllegalStateException("MemoryJournal() requires Web Crypto; supply an explicit runId")
    RunGlobals.crypto.randomUUID()
