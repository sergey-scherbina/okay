```scala
import okay.script.api.*
```
<nav>
  <a href="/">Okay Store</a> ·
  <a href="/live">Live</a> ·
  <a href="/cart">Cart (${Session.current.get("cart").map(_.split(",").length).getOrElse(0)})</a>
</nav>
