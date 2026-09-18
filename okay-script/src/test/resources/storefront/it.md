[Service, Shop](/lib/domain.md)

[serviceCard](/lib/cards.md)

[storefrontStyle](/lib/theme.md)

```scala declare
val itShop = Shop("site-it", "it", "IT consulting", false, "#3b82f6", Vector(
  Service("cicd", "Turnkey CI/CD", "build · tests · deploy", 0, "site-it"),
  Service("ai", "AI that solves problems", "a tool under your control", 0, "site-it")))
```
<!doctype html><html><head><meta charset="utf-8"><title>${itShop.title}</title>
<style>${storefrontStyle(itShop.clothing, itShop.accent)}</style></head><body>
<div class="bg"></div><div class="glow"></div>
<div class="wrap">
  <div class="top"><span class="mark">${itShop.title}</span></div>
  <section class="hero"><p class="kicker">Systems · diagnosed &amp; solved</p>
    <h1 class="headline">Systems that answer for themselves</h1></section>
  <section class="offers">${itShop.services.map(sv => serviceCard(sv, itShop.slug)).mkString}</section>
</div></body></html>
