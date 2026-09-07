```scala declare
// included by index.md, product/[sku].md and admin.md: the catalog
// lives in the APPLICATION scope (shared by every page; survives a
// restart when the Site's Application is persisted), seeded here once
// from a default. Each page is its own compilation unit, so this
// Product is this include's own class -- pages trade its JSON, never
// the object.
import okay.script.api.*
import okay.codec.Schema
final case class Product(sku: String, name: String, price: Int)
given Schema[Product] = Schema.derived
val defaultCatalog = Vector(
  Product("ok-1", "Effect row, one row", 10),
  Product("ok-2", "Direct style, no ceremony", 25),
  Product("ok-3", "Markdown that compiles", 40),
)
```
```scala
// the include prints nothing; it seeds on the first render of any
// page that includes it
if Application.current.get("catalog").isEmpty then Application.current.put("catalog", defaultCatalog)
```
