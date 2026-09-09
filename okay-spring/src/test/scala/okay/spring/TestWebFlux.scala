package okay.spring

import okay.*
import org.springframework.context.annotation.{AnnotationConfigApplicationContext, Configuration}
import org.springframework.web.bind.annotation.{GetMapping, RestController}
import org.springframework.web.reactive.config.EnableWebFlux
import org.springframework.test.web.reactive.server.WebTestClient

/** a controller that returns a program, not a Mono */
@RestController
class HelloController:
  @GetMapping(Array("/hello"))
  def hello(): String ! Async = async("hi").map(_ + "!")

@Configuration
@EnableWebFlux
class WebFluxConfig

/** specs/di.md stage 2: the adapter through the real WebFlux handler
 * stack, from a context the auto-configuration configured (no server) */
class TestWebFlux extends munit.FunSuite {
  test("a controller returning A ! Async is served by WebFlux through the auto-configured registry") {
    val ctx = AnnotationConfigApplicationContext(classOf[WebFluxConfig], classOf[OkayAutoConfiguration], classOf[HelloController])
    try
      WebTestClient.bindToApplicationContext(ctx).build()
        .get().uri("/hello").exchange()
        .expectStatus().isOk
        .expectBody(classOf[String]).isEqualTo("hi!")
      ()
    finally ctx.close()
  }
}
