---
title: Okay Store
---
```scala
import okay.script.api.*
import okay.codec.Schema
include("parts/header.md")
include("parts/catalog.md")
final case class Product(sku: String, name: String, price: Int)
given Schema[Product] = Schema.derived
val catalog = Application.current.value[Vector[Product]]("catalog").getOrElse(Vector.empty)
```
<link rel="stylesheet" href="/style.css">

# ${okay.script.Meta.current("title")}

<ul>
```scala
for p <- catalog do
  println(s"""  <li><a href="/product/${p.sku}">${p.name}</a> — $$${p.price}</li>""")
```
</ul>
