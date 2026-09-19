# the editor

The same `Schema` that renders the storefront renders the form that
edits it, and reads the post back (okay-script-forms). A page, not a
feature.

[Shop](/lib/domain.md)

[shop, shipped](/lib/content.md)

```scala
import okay.script.api.*

if Web.current.method == "POST" && Web.current.form.contains("__reset") then
  Content.clear("content/shop.json"): Unit
  println("<p class=\"said\">reset to what shipped</p>")
else if Web.current.method == "POST" then
  Forms.read[Shop](Web.current.form) match
    case Right(edited) =>
      Content.write("content/shop.json", edited): Unit
      println("<p class=\"said\">saved " + edited.services.size + " services</p>")
    case Left(draft) =>
      println(Forms.html[Shop]("/edit", draft, submit = "Save"))
else
  val current = okay.codec.Json.parse(okay.codec.Json.write(shop))
  println(Forms.html[Shop]("/edit", Forms.Draft(current, Vector.empty), submit = "Save"))
```
