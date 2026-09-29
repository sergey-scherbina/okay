- [ ] xml-strict-void — the XML dialect applies HTML's VOID set (`br`,
      `img`, `source`, …) to every document, so `<source>Coal</source>`
      in an FpML coal swap never opens and its close "closes nothing":
      three of okay-fin's 801 corpus documents are declined by xml on
      it (2026-09-29). THE ASK: `Xml.stepWith(void)` and
      `Xml.cst(input, void)` with the HTML set as the default (nothing
      existing changes), `Xml.strict` = no voids; okay-refine's
      `Format.xml` reads STRICT — it detects XML data documents, and an
      HTML page's `<br>` declining as "unclosed" is the right answer
      there. okay2 in step. (2026-09-29, from okay-fin's corpus run)
