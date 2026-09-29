## xml-strict-void - the XML dialect can run without HTML's void elements; Format.xml does

- `Xml.stepWith(voids)`, `Xml.cst(input, voids)`, `Xml.strict` (no void
  element) beside the HTML-void default, which is unchanged; okay2 in
  step. Found by okay-fin's FpML corpus: `<source>Coal</source>` in a
  coal swap never opened under the HTML set (`source` is an HTML void)
  and its close "closed nothing" — three of 801 documents declined.
- okay-refine's `Format.xml` reads STRICT: it detects XML data documents,
  and an HTML page's `<br>` declining as "never closed" is the right
  answer there (TestFormat pins both directions).
