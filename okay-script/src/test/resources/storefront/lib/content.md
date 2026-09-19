---
exports:
  - shop
  - shipped
route: false
---

# the shop, as content

The words a storefront shows are data the owner edits, not code
(specs/site-framework.md stage 2). `shipped` is what the site ships
with; `shop` is that, unless the owner has edited the file.

[Service, Shop](/lib/domain.md)

```scala declare
import okay.script.api.Content

val shipped = Shop("site-szykownia", "szykownia", "Szykownia", true, "#9e1042", Vector(
  Service("hem", "Skrócenie spodni", "wyślij InPost, 3-5 dni", 3500, "site-szykownia"),
  Service("zipper", "Wymiana zamka", "kurtki, torebki", 6000, "site-szykownia"),
  Service("bespoke", "Szycie na miarę", "", 0, "site-szykownia")))

def shop: Shop = Content.read[Shop]("content/shop.json", shipped)
```
