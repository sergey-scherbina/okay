---
title: Okay Store
---
```scala
import okay.script.api.*
include("parts/header.md")
```
<link rel="stylesheet" href="/style.css">

# ${okay.script.Meta.current("title")}

```scala declare
// object-level (JSP <%! %>): built once per compile, shared by every request
val catalog: Vector[(String, String, Int)] = Vector(
  ("ok-1", "Effect row, one row", 10),
  ("ok-2", "Direct style, no ceremony", 25),
  ("ok-3", "Markdown that compiles", 40),
)
def priceOf(sku: String): Option[Int] = catalog.collectFirst { case (s, _, p) if s == sku => p }
```

<ul>
```scala
for (sku, name, price) <- catalog do
  println(s"""  <li><a href="/product/$sku">$name</a> — $$$price</li>""")
```
</ul>
