```scala
import okay.script.api.*
include("../parts/header.md")
val sku = Web.current.params("sku")
```
<link rel="stylesheet" href="/style.css">

```scala
priceOf(sku) match
  case None =>
    Response.current.status = 404
    println(s"<p>No such product: $sku</p>")
  case Some(price) =>
    println(s"<h1>$sku</h1><p>$$$price</p>")
    println(s"""<form method="post" action="/cart"><input type="hidden" name="sku" value="$sku"><button>Add to cart</button></form>""")
```

```scala declare
// each page is its own compilation unit -- a declare block here is this page's own
def priceOf(sku: String): Option[Int] =
  Map("ok-1" -> 10, "ok-2" -> 25, "ok-3" -> 40).get(sku)
```
