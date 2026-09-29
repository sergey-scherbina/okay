- [ ] xml-from-value — `Xml.fromValue(json): String`, the inverse of
      `Xml.value` (refine-fpml-prover): a one-field root object is the
      document element, `@name` fields its attributes, `#text` its text,
      an array a repeated element, a string escaped. Law:
      `Xml.value(Xml.cst(Xml.fromValue(v), Xml.strict)) == v` for every
      value `Xml.value` produces. okay2 in step. Consumer: okay-fin's
      `Convert` — a swap read from CDM JSON or an MT360 written as an
      FpML document (okay-fin BACKLOG `fin-convert` was blocked on this).
      Additive: own suites + `affected master Test/compile`. (2026-09-29)
