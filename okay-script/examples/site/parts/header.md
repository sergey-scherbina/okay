```scala
import okay.script.api.*
val other = if Lang.current == "uk" then "en" else "uk"
```
<nav>
  <a href="/">${t("nav.store")}</a> ·
  <a href="/live">${t("nav.live")}</a> ·
  <a href="/cart">${t("nav.cart", Session.current.get("cart").map(_.split(",").length).getOrElse(0))}</a> ·
  ${Principal.current.map(p => s"""<a href="/admin">${t("nav.admin")} (${p.id})</a> · <a href="/logout">${t("nav.signout")}</a>""").getOrElse(s"""<a href="/admin">${t("nav.admin")}</a>""")} ·
  <a href="?lang=${other}">${t("lang.switch")}</a>
</nav>
