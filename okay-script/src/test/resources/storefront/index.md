[Service, Shop](/lib/domain.md)

[serviceCard, storefrontStyle](/lib/cards.md)

[shop](/lib/content.md)

[langs](/lib/i18n.md)

<!doctype html><html><head><style>${storefrontStyle(shop.clothing, shop.accent)}</style></head>
<body>${okay.script.api.Inline.switcher(langs)}
<h1>${okay.script.api.Inline.span(langs, "pl" -> "Szykownia", "en" -> "The atelier", "uk" -> "Ательє")}</h1>
${shop.services.map(sv => serviceCard(sv, shop.slug)).mkString}
${okay.script.api.Inline.script(langs)}</body></html>
