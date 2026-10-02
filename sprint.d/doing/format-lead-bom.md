- [ ] format-lead-bom — format-cheap-decline (6484805e8) made `Format.xml`
      decline a document that begins with a UTF-8 byte-order mark, which
      the XML parser itself accepts: okay-fin's FpML corpus gate found it
      (cds-index-tranche.xml, 1 of 801). `lead` skips a leading U+FEFF;
      TestFormat pins an XML document with a BOM as text/xml. (2026-09-29)
