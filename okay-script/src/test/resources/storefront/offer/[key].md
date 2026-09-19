# the offer screen

One ready-made offer, and the form that orders it. The page is a
PARAMETER — `/offer/hem` is this file with `key = hem` — and the form
comes from the `Intake` Schema, so what it asks and what the server
reads cannot drift apart.

[Shop](/lib/domain.md)

[storefrontStyle](/lib/theme.md)

[shop](/lib/content.md)

[money, priceShort](/lib/money.md)

[esc](/lib/html.md)

[Intake, checks, blank](/lib/intake.md)

```scala declare
import okay.script.api.*
import okay.codec.Json

def ordered(i: Intake): String =
  "R-" + math.abs((i.contact + i.need).hashCode % 100000)
```
```scala
val key = Web.current.params.getOrElse("key", "")
val offer = shop.services.find(_.key == key)

println("<!doctype html><html><head><meta charset=\"utf-8\">")
println("<title>" + esc(offer.map(_.name).getOrElse("—")) + "</title>")
println("<style>" + storefrontStyle(shop.clothing, shop.accent) + "</style></head><body>")
println("<div class=\"bg\"><i class=\"seam s1\"></i><i class=\"seam s3\"></i></div><div class=\"glow\"></div>")
println("<div class=\"wrap\">")
println("<div class=\"top\"><a class=\"mark\" href=\"/\">" + esc(shop.title) + "</a></div>")

offer match
  case None =>
    Response.current.status = 404
    println("<section class=\"hero\"><h1 class=\"headline\">Nie ma takiej oferty</h1></section>")
  case Some(sv) =>
    println("<section class=\"hero\"><p class=\"label\">Zamówienie</p>")
    println("<h1 class=\"headline\">" + esc(sv.name) + "</h1>")
    if sv.description.nonEmpty then println("<p class=\"lead\">" + esc(sv.description) + "</p>")
    println("<p class=\"sla\">" + esc(priceShort(sv.priceCents)) + "</p></section>")
    println("<section class=\"formcard\">")
    if Web.current.method == "POST" then
      Forms.read[Intake](Web.current.form, checks*) match
        case Right(i) =>
          println("<div class=\"done\"><p class=\"big\">Dziękuję — odezwę się.</p>")
          println("<p>" + esc(ordered(i)) + "</p></div>")
        case Left(draft) =>
          println("<p class=\"label\">Popraw i wyślij jeszcze raz</p>")
          println(Forms.html[Intake]("/offer/" + esc(key), draft, submit = "Wyślij"))
    else
      println(Forms.html[Intake]("/offer/" + esc(key), Forms.Draft(blank, Vector.empty), submit = "Wyślij"))
    println("</section>")

println("</div></body></html>")
```
