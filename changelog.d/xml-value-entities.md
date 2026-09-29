## xml-value-entities - Xml.value decodes entities

- `Xml.unescape`: the five predefined entities and numeric character
  references (`&#169;`, `&#x1F600;`), an unknown entity or a bare `&`
  left as written; `Xml.value` applies it to text and attribute values.
  The lossless tree and `Xml.text` are untouched. okay2 in step.
- Found by okay-fin's cross-format law: `<indexName>S&amp;P…` read as
  `S&amp;P` where the CDM twin says `S&P`.
