- [ ] xml-projection-stack-safe — okay-codec's `Xml.text` and
      `Xml.elements` recurse per nesting level (`kids.map(text)`,
      `kids.flatMap(elements(_, name))`), the same defect
      `cst-walk-stack-safe` fixed in okay-parse: `Xml.cst` builds a
      20 000-deep `<a>…</a>` document without trouble and the
      projections then overflow. Found porting to okay2 (okay2-xml,
      2026-09-25), where both walk an explicit stack and a recursive
      mutant throws StackOverflowError at that depth. The fix is the
      same pre-order walk, with the depth test asserting both.
      (2026-09-25)
