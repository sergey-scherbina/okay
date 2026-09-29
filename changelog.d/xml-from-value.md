## xml-from-value - Xml.fromValue, the way back to a document

- `Xml.fromValue(json): String`, the inverse of `Xml.value`: a field an
  element, `@name` an attribute, `#text` the text, an array a repeated
  element, strings escaped (`Xml.escape`). Law, tested:
  `Xml.value(Xml.cst(Xml.fromValue(v), Xml.strict)) == v` for every value
  `Xml.value` produces. An explicit worklist: 100 000 nested elements
  write without a stack. okay2 in step.
- Consumer: okay-fin's `Convert` — a swap read from CDM JSON or an MT360
  written as an FpML document; `fin-convert` was blocked on this.
