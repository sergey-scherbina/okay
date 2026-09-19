---
exports: [money, priceShort]
route: false
---

# money

```scala declare
def money(cents: Long): String = f"${cents / 100.0}%.2f zł"
def priceShort(cents: Long): String = if cents <= 0 then "wycena" else money(cents)
```
