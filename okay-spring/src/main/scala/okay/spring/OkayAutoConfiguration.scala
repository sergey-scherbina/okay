package okay.spring

import org.springframework.beans.factory.config.BeanPostProcessor
import org.springframework.boot.autoconfigure.AutoConfiguration
import org.springframework.boot.autoconfigure.condition.ConditionalOnBean
import org.springframework.context.annotation.{Bean, Import}
import org.springframework.core.ReactiveAdapterRegistry
import org.springframework.web.reactive.result.method.annotation.ResponseBodyResultHandler

/**
 * The Boot starter's whole content: with okay-spring on the classpath,
 * a controller may return `A ! Async`.
 *
 * WebFlux does NOT consult `ReactiveAdapterRegistry.getSharedInstance`:
 * `WebFluxConfigurationSupport` makes a registry BEAN of its own, and
 * a program registered only on the shared one reaches Jackson, which
 * tries to serialise the program (measured: "No serializer found for
 * class okay.Free$Bind"). So the registrar is a `BeanPostProcessor`
 * that teaches every registry bean as it is created — and the shared
 * instance too, for whoever asks it directly.
 */
@AutoConfiguration
@Import(Array(classOf[OkayReactiveRegistrar]))
class OkayAutoConfiguration:
  @Bean
  def okayReactiveAdapter(): OkayReactiveAdapter = OkayReactiveAdapter()

  /** the result handler, only where WebFlux is configured (see OkayResultHandler for why) */
  @Bean
  @ConditionalOnBean(Array(classOf[ResponseBodyResultHandler]))
  def okayResultHandler(delegate: ResponseBodyResultHandler): OkayResultHandler = OkayResultHandler(delegate)

/** every `ReactiveAdapterRegistry` bean learns the program type */
class OkayReactiveRegistrar extends BeanPostProcessor:
  OkayReactive.registerAdapter(ReactiveAdapterRegistry.getSharedInstance)
  override def postProcessAfterInitialization(bean: AnyRef, name: String): AnyRef =
    bean match
      case r: ReactiveAdapterRegistry => OkayReactive.registerAdapter(r)
      case _ => ()
    bean

/** the marker bean: present when the auto-configuration ran */
final class OkayReactiveAdapter
