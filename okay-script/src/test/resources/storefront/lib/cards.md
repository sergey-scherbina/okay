---
exports: [serviceCard]
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
```
