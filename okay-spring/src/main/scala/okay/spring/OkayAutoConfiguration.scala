package okay.spring

import org.springframework.boot.autoconfigure.AutoConfiguration
import org.springframework.context.annotation.Bean
import org.springframework.core.ReactiveAdapterRegistry

/** the Boot starter's whole content: with okay-spring on the classpath,
 * a controller may return `A ! Async` */
@AutoConfiguration
class OkayAutoConfiguration:
  @Bean
  def okayReactiveAdapter(): OkayReactiveAdapter =
    OkayReactive.registerAdapter(ReactiveAdapterRegistry.getSharedInstance)
    OkayReactiveAdapter()

/** the marker bean: present when the adapter has been registered */
final class OkayReactiveAdapter
