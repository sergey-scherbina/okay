[Service, Shop](/lib/domain.md)

[serviceCard, storefrontStyle](/lib/cards.md)

```scala declare
val it = Shop("site-it", "it", "IT consulting", false, "#3b82f6", Vector(
  Service("cicd", "Turnkey CI/CD", "build · tests · deploy", 0, "site-it")))
```
<style>${storefrontStyle(it.clothing, it.accent)}</style>
${it.services.map(sv => serviceCard(sv, it.slug)).mkString}
