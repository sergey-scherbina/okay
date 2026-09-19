---
exports:
  - Service
  - Shop
route: false
---

# the domain — what a storefront is made of

A `Service` is one thing an atelier does; a `Shop` is the site that
offers them. Both cross module boundaries as TYPES, which is what
makes the rest of these pages ordinary Scala.

```scala declare
final case class Service(key: String, name: String, description: String,
                         priceCents: Long, siteId: String)
final case class Shop(id: String, slug: String, title: String,
                      clothing: Boolean, accent: String,
                      services: Vector[Service])
```
