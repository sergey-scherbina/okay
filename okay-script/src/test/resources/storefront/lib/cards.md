---
exports: [serviceCard, storefrontStyle]
route: false
---

# the storefront's own furniture

[Service](/lib/domain.md)

[i18nAttrs](/lib/i18n.md)

[priceShort](/lib/money.md)

[esc](/lib/html.md)

```scala declare
def serviceCard(sv: Service, slug: String): String =
  val desc =
    if sv.description.isEmpty then ""
    else "<span class=\"offer-desc\">" + esc(sv.description) + "</span>"
  "<a class=\"offer\" href=\"/s/" + slug + "/offer/" + sv.key + "\">" +
    "<span class=\"offer-main\"><span class=\"offer-name\"" +
    i18nAttrs(sv.name, sv.name, sv.name, sv.name) + ">" + esc(sv.name) + "</span>" + desc + "</span>" +
    "<span class=\"offer-price\">" + priceShort(sv.priceCents) + "</span>" +
    "<span class=\"offer-go\">&rarr;</span></a>"

def storefrontStyle(clothing: Boolean, accent: String): String =
  val root =
    if clothing then "--void:#dcc4b6;--ink:#3a2a22;--surface:#efe3dc"
    else "--void:#05070c;--ink:#eaf1fb;--surface:#0c1420"
  ":root{--accent:" + accent + ";" + root + "}" +
    "body{margin:0;background:var(--void);color:var(--ink)}" +
    ".offer{display:flex;gap:12px;padding:14px 16px;text-decoration:none;color:inherit}"
```
