```scala
import okay.script.api.*
```
<nav>
  <a href="/">Okay Store</a> ·
  <a href="/live">Live</a> ·
  <a href="/cart">Cart (${Session.current.get("cart").map(_.split(",").length).getOrElse(0)})</a> ·
  ${Principal.current.map(p => s"""<a href="/admin">Admin (${p.id})</a> · <a href="/logout">Sign out</a>""").getOrElse("""<a href="/admin">Admin</a>""")}
</nav>
