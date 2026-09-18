---
exports: [esc]
route: false
---

# html — escaping, the one thing every renderer needs

```scala declare
def esc(s: String): String =
  s.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;").replace("\"", "&quot;")
```
