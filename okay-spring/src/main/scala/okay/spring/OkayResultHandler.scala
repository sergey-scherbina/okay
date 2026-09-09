package okay.spring

import okay.*
import org.springframework.core.{MethodParameter, Ordered, ResolvableType}
import org.springframework.web.reactive.{HandlerResult, HandlerResultHandler}
import org.springframework.web.reactive.result.method.annotation.ResponseBodyResultHandler
import org.springframework.web.server.ServerWebExchange
import reactor.core.publisher.Mono

/**
 * Why the adapter alone is not enough: WebFlux reads a reactive return
 * type's ELEMENT type from generic index 0, and `A ! Async` is
 * `Free[Async, A]` — index 0 is the effect. Measured: with only the
 * adapter, a `String ! Async` was written as a server-sent event
 * (`data:hi!`), the element type having resolved to `Async`. This
 * handler runs just before `ResponseBodyResultHandler`, turns the
 * program into a `Mono`, and hands it on under a stand-in return type
 * whose index 0 says what index 1 said: `Mono[String]` for a
 * CharSequence result (text, as Spring writes strings), `Mono[Object]`
 * for anything else (encoded by its runtime class — JSON).
 */
final class OkayResultHandler(delegate: ResponseBodyResultHandler) extends HandlerResultHandler, Ordered:

  override def getOrder: Int = delegate.getOrder - 1

  override def supports(result: HandlerResult): Boolean =
    classOf[Free[?, ?]].isAssignableFrom(result.getReturnTypeSource.getParameterType) && delegate.supports(result)

  override def handleResult(exchange: ServerWebExchange, result: HandlerResult): Mono[Void] =
    val elem = ResolvableType.forMethodParameter(result.getReturnTypeSource).getGeneric(1).resolve(classOf[AnyRef])
    val stub = if classOf[CharSequence].isAssignableFrom(elem) then OkayResultHandler.string else OkayResultHandler.any
    delegate.handleResult(exchange, HandlerResult(result.getHandler, program(result.getReturnValue), stub, result.getBindingContext))

  /** the value arrives as Object, its class checked by `supports` above;
   * the pattern restates that check — an erased type behind a wildcard */
  private def program(value: AnyRef): Mono[?] = value match
    case p: Free[Async, AnyRef] @unchecked => OkayReactive.mono(p)
    case null => Mono.empty()
    case other => throw IllegalStateException(s"not an okay program: ${other.getClass.getName}")

object OkayResultHandler:
  /** the stand-in signatures Spring reads the element type from */
  class Stubs:
    def string(): Mono[String] = Mono.empty()
    def any(): Mono[AnyRef] = Mono.empty()
  private[spring] val string = MethodParameter(classOf[Stubs].getMethod("string"), -1)
  private[spring] val any = MethodParameter(classOf[Stubs].getMethod("any"), -1)
