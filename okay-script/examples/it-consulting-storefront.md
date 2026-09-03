---
tagline: Решаю любые IT-проблемы любого масштаба. Адекватный подход. Цена договорная.
contact: Напишите — отвечаю в течение дня. Консультация по записи.
---

# IT Consulting — storefront

A worked example for `okay-script` (specs/okay-script.md, "Worked
example"): a real `okay-jetty` server, a storefront page and an
`/order` route, compiled and RUN AT RUNTIME by `ScalaScript.run` from
this file's own ```scala block below — nothing here is a custom
language or interpreter, it is plain Scala.

The services list (name, description, price, currency) is taken
verbatim from `../it-consulting/site/site.md`, the real IT-consulting
business line's own site content. That content is normally rendered by
a different system (`busi`'s declarative-site engine) and its `/order`
behavior is normally a `scalascript` snippet (`on order: receive job;
line work; line delivery`) — neither is reused here. Only the DATA
crosses over; the page and the order handler below are ordinary Scala.

The `tagline`/`contact` above are real front-matter (okay-script-meta,
specs/okay-script.md "Metadata as context") — the page below reads
them via `okay.script.Meta.current`, the same way `site.md`'s own
front-matter feeds `busi`'s rendering, instead of hardcoding them a
second time as Scala string literals.

```scala
import okay.*
import okay.given
import okay.jetty.Jetty
import okay.http.{Request, Response, Http, Server as OkayServer}
import okay.script.Meta

final case class Service(key: String, name: String, description: String, price: Double, currency: String)

val services = Vector(
  Service("audit",   "Добавление новых функций и развитие",     "",                                4500.00, "PLN"),
  Service("review",  "Исправление ошибок в системе",             "",                                1800.00, "PLN"),
  Service("consult", "Консультация",                             "",                                 350.00, "PLN"),
  Service("cicd",    "Настройка CI/CD под ключ",                 "сборка · тесты · деплой",             0.00, "PLN"),
  Service("ai",      "Искусственный интеллект, который решает",  "инструмент под вашим контролем",      0.00, "PLN"),
)

def priceOf(s: Service): String =
  if s.price == 0.00 then "по договорённости" else f"${s.price}%.2f ${s.currency}"

def card(s: Service): String =
  s"""<div class="card">
     |  <h3>${s.name}</h3>
     |  <p>${s.description}</p>
     |  <p class="price">${priceOf(s)}</p>
     |  <a class="order" href="/order/${s.key}">Заказать</a>
     |</div>""".stripMargin

def page: String =
  s"""<!doctype html>
     |<meta charset="utf-8">
     |<title>IT Consulting</title>
     |<style>
     |  body { font: 15px/1.5 system-ui, sans-serif; background: #10141a; color: #e6e9ef; margin: 0; }
     |  main { max-width: 720px; margin: 0 auto; padding: 2rem 1rem; }
     |  h1 { font-size: 1.4rem; }
     |  .tagline { color: #9fb0c8; margin-bottom: 2rem; }
     |  .card { background: #171c25; border-radius: .8rem; padding: 1rem 1.2rem; margin-bottom: .8rem; }
     |  .price { color: #6b9fff; font-weight: 600; }
     |  .order { color: white; background: #3563a8; padding: .4rem .8rem; border-radius: .5rem; text-decoration: none; display: inline-block; }
     |  .contact { color: #7a869c; margin-top: 2rem; font-size: .9em; }
     |</style>
     |<main>
     |  <h1>IT Consulting</h1>
     |  <p class="tagline">${Meta.current("tagline")}</p>
     |  ${services.sortBy(_.key).map(card).mkString("\n  ")}
     |  <p class="contact">${Meta.current("contact")}</p>
     |</main>""".stripMargin

def orderPage(s: Service): String =
  s"""<!doctype html><meta charset="utf-8"><title>Заявка принята</title>
     |<body style="font:15px system-ui,sans-serif;background:#10141a;color:#e6e9ef;padding:2rem">
     |<p>Заявка на «${s.name}» принята. ${priceOf(s)}.</p>
     |<p><a style="color:#6b9fff" href="/">&larr; назад</a></p>
     |</body>""".stripMargin

def html(status: Int, body: String): Response ! Async =
  pure(Response(status, Seq("content-type" -> "text/html; charset=utf-8"), Http.one(body.getBytes("UTF-8"))))

// port is a system property, not a literal, so a caller (a test, a
// generator) can bind a fresh one without editing this file
val port = sys.props.get("okay.script.storefront.port").flatMap(_.toIntOption).getOrElse(8099)

Resource.run[Unit, Pure](
  Jetty.serve(port) {
    case r if OkayServer.path(r) == "/" =>
      html(200, page)
    case r if r.url.startsWith("/order/") =>
      val key = r.url.stripPrefix("/order/")
      services.find(_.key == key) match
        case Some(s) =>
          println(s"ORDER: ${s.key} (${s.name})")
          html(200, orderPage(s))
        case None =>
          OkayServer.text(404, "unknown service")
  }().map { s =>
    println(s"storefront: http://127.0.0.1:${Jetty.port(s)}")
    Thread.sleep(Long.MaxValue)   // interrupt this thread to stop the server
  }
).runWith
```
