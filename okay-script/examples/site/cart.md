```scala
import okay.script.api.*
// a POSTed sku adds to the cart, then POST-redirect-GET; `/clear`
// FORWARDS here (still a POST, but with no sku), so the check is on
// the form, not the method
Web.current.form.get("sku").foreach { s =>
  val items = Session.current.get("cart").map(_.split(",").toVector).getOrElse(Vector.empty)
  Session.current.set("cart", (items :+ s).mkString(","))
  Response.current.redirect("/cart")   // the output below is never sent
}
include("parts/header.md")
```
<link rel="stylesheet" href="/style.css">

# Cart

```scala
Session.current.get("cart") match
  case None => println("<p>Empty.</p>")
  case Some(csv) =>
    println("<ul>")
    csv.split(",").foreach(s => println(s"  <li>$s</li>"))
    println("</ul>")
    println("""<form method="post" action="/clear"><button>Clear</button></form>""")
```
