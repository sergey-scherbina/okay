[Service, Shop](/lib/domain.md)

[serviceCard, storefrontStyle](/lib/cards.md)

[langs](/lib/i18n.md)

```scala declare
val szykownia = Shop("site-szykownia", "szykownia", "Szykownia", true, "#9e1042", Vector(
  Service("hem", "Skrócenie spodni", "wyślij InPost, 3-5 dni", 3500, "site-szykownia"),
  Service("zipper", "Wymiana zamka", "kurtki, torebki", 6000, "site-szykownia"),
  Service("bespoke", "Szycie na miarę", "", 0, "site-szykownia")))
```
<!doctype html><html><head><style>${storefrontStyle(szykownia.clothing, szykownia.accent)}</style></head>
<body>${okay.script.api.Inline.switcher(langs)}
<h1>${okay.script.api.Inline.span(langs, "pl" -> "Szykownia", "en" -> "The atelier", "uk" -> "Ательє")}</h1>
${szykownia.services.map(sv => serviceCard(sv, szykownia.slug)).mkString}
${okay.script.api.Inline.script(langs)}</body></html>
