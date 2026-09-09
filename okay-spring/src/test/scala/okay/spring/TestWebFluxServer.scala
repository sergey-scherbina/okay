package okay.spring

import org.springframework.boot.{SpringBootConfiguration, WebApplicationType}
import org.springframework.boot.autoconfigure.EnableAutoConfiguration
import org.springframework.boot.builder.SpringApplicationBuilder
import org.springframework.web.reactive.function.client.WebClient

/** the Boot application of the end-to-end: auto-configuration on, one controller */
@SpringBootConfiguration
@EnableAutoConfiguration
class HelloApp

/**
 * specs/di.md stage 2, the real thing: Boot starts Netty on a random
 * port with okay-spring's auto-configuration discovered from
 * META-INF/spring, and an HTTP client reads what the controller's
 * program produced. Binds a port, so `Live` (integration-test-gate).
 */
class TestWebFluxServer extends munit.FunSuite {
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))

  test("Boot + auto-configuration + Netty: GET /hello answers the program's value") {
    val ctx = SpringApplicationBuilder(classOf[HelloApp], classOf[HelloController])
      .web(WebApplicationType.REACTIVE).run("--server.port=0")
    try
      val port = ctx.getEnvironment.getProperty("local.server.port")
      val body = WebClient.create(s"http://localhost:$port").get().uri("/hello")
        .retrieve().bodyToMono(classOf[String]).block()
      assertEquals(body, "hi!")
      assert(ctx.getBean(classOf[OkayReactiveAdapter]) != null)
    finally ctx.close()
  }
}
