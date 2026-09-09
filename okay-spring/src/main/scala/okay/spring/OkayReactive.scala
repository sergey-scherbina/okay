package okay.spring

import okay.*
import okay.given
import org.reactivestreams.Publisher
import org.springframework.core.{ReactiveAdapterRegistry, ReactiveTypeDescriptor}
import reactor.core.publisher.Mono
import reactor.core.scheduler.Schedulers
import java.util.function.{Function as JFunction, Supplier}

/**
 * `A ! Async` as a reactive type Spring understands: a controller
 * returns the program, and WebFlux subscribes to it as it would to a
 * Mono. The program runs on Reactor's blocking-friendly scheduler —
 * it may park, and a virtual thread inside parks wherever it blocks.
 */
object OkayReactive:

  /** run the program on subscription, once per subscriber */
  def mono[A](p: => A ! Async): Mono[A] =
    Mono.fromCallable(() => p.runWith).subscribeOn(Schedulers.boundedElastic())

  /** a publisher's single value as an Async operation: the thread parks for it */
  def fromPublisher[A](pub: Publisher[A]): A ! Async =
    async(Mono.from(pub).block())

  /** the type Spring sees: `Free`, the class every program is an instance of */
  val programClass: Class[?] = classOf[Free[?, ?]]

  /** teach a registry to adapt programs — the shared instance is what WebFlux consults */
  def registerAdapter(registry: ReactiveAdapterRegistry = ReactiveAdapterRegistry.getSharedInstance): Unit =
    if registry.getAdapter(programClass) == null then
      registry.registerReactiveType(
        ReactiveTypeDescriptor.singleOptionalValue(programClass, (() => pure[Async, Null](null)): Supplier[AnyRef]),
        toPublisher,
        ((pub: Publisher[?]) => fromPublisher(pub)): JFunction[Publisher[?], AnyRef])

  /** the registry hands the value as Object after checking its class
   * against `programClass`; the pattern below restates that check for
   * the compiler — an erased type behind a wildcard, the one cast this
   * module makes */
  private val toPublisher: JFunction[Any, Publisher[?]] =
    case p: Free[Async, Any] @unchecked => mono(p)
    case other => throw IllegalArgumentException(s"not an okay program: ${other.getClass.getName}")
