```scala declare
// a declare block is object level: its imports and givens are its own
import okay.script.api.*
import okay.codec.Schema
final case class Order(name: String, email: String, qty: Int, gift: Boolean)
given Schema[Order] = Schema.derived
val inStock: okay.ui.Form.Check[Order] = o => if o.qty <= 5 then Vector.empty else Vector("qty" -> "only 5 in stock")
```
```scala
import okay.script.api.*
include("parts/header.md")
// the plain road: a form from the Schema, read back through the same Schema
val posted = if Web.current.method == "POST" then Forms.read[Order](Web.current.form, inStock) else Left(Forms.Draft.empty)
```
<link rel="stylesheet" href="/style.css">

# Checkout

```scala
posted match
  case Right(o) =>
    Session.current.remove("cart")
    println(s"<p>Thanks, ${o.name}! ${o.qty} item(s) on the way${if o.gift then ", gift-wrapped" else ""}.</p>")
  case Left(draft) =>
    println(Forms.html[Order]("/checkout", draft, "Place order"))
```
