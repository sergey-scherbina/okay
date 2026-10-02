## format-lead-bom - the first-character test skips a byte-order mark

- format-cheap-decline (6484805e8) declined an XML document beginning with
  a UTF-8 BOM, which the XML parser accepts: found by okay-fin's FpML
  corpus gate (cds-index-tranche.xml). `Format`'s first-character test
  skips a leading U+FEFF again; TestFormat pins an XML document with one.
