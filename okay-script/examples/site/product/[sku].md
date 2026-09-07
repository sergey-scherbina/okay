```scala
import okay.script.api.*
import okay.codec.Schema
include("../parts/header.md")
include("../parts/catalog.md")
final case class Product(sku: String, name: String, price: Int)
given Schema[Product] = Schema.derived
val sku = Web.current.params("sku")
val found = Application.current.value[Vector[Product]]("catalog").getOrElse(Vector.empty).find(_.sku == sku)
```
<link rel="stylesheet" href="/style.css">

```scala
found match
  case None =>
    Response.current.status = 404
    println(s"<p>No such product: $sku</p>")
  case Some(p) =>
    println(s"<h1>${p.name}</h1><p>${p.sku} — $$${p.price}</p>")
    println(s"""<form method="post" action="/cart"><input type="hidden" name="sku" value="$sku"><button>Add to cart</button></form>""")
```
