# IT Consulting — price list (a `render` example)

Same service data as `examples/it-consulting-storefront.md`, this
time rendered as a plain document (not served) — the "JSP but
Scala+Markdown" case: prose with Scala values dropped straight in,
via `ScalaScript.render`.

```scala
final case class Service(key: String, name: String, price: Double, currency: String)

val services = Vector(
  Service("audit",   "Добавление новых функций и развитие", 4500.00, "PLN"),
  Service("review",  "Исправление ошибок в системе",         1800.00, "PLN"),
  Service("consult", "Консультация",                          350.00, "PLN"),
)

def priceOf(s: Service): String =
  if s.price == 0.00 then "по договорённости" else f"${s.price}%.2f ${s.currency}"
```

Всего услуг в прайсе: ${services.size}.
Самая дорогая: ${services.maxBy(_.price).name} (${priceOf(services.maxBy(_.price))}).

## Полный список

${services.map(s => s"- ${s.name} — ${priceOf(s)}").mkString("\n")}
