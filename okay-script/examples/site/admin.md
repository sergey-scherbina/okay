---
secure: admin
---
```scala declare
import okay.script.api.*
import okay.codec.Schema
final case class Product(sku: String, name: String, price: Int)
given Schema[Product] = Schema.derived
def catalog: Vector[Product] = Application.current.value[Vector[Product]]("catalog").getOrElse(Vector.empty)
def add(p: Product): String =
  Application.current.put("catalog", catalog.filterNot(_.sku == p.sku) :+ p)
  s"saved ${p.sku}"
val priced: okay.ui.Form.Check[Product] = p => if p.price > 0 then Vector.empty else Vector("price" -> "must be positive")
// the live road: adds without a reload
val adder = Live.formWith[Product]("Save")(add, priced)
```
```scala
import okay.script.api.*
include("parts/header.md")
include("parts/catalog.md")
// the plain road: the same Product, posted
val posted = if Web.current.method == "POST" then Forms.read[Product](Web.current.form, priced) else Left(Forms.Draft.empty)
val saved = posted.toOption.map(add)
```
<link rel="stylesheet" href="/style.css">

# Admin — ${Principal.current.map(_.id).getOrElse("?")}

<ul>
```scala
for p <- catalog do println(s"  <li>${p.sku}: ${p.name} — $$${p.price}</li>")
```
</ul>

## Add or replace a product (live)

${mount("adder", adder)}

## The same, as a plain form

```scala
saved.foreach(m => println(s"<p>$m</p>"))
println(Forms.html[Product]("/admin", posted.left.getOrElse(Forms.Draft.empty), "Save"))
```
