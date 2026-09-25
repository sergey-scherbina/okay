## xml-projection-stack-safe - okay-codec's Xml projections no longer overflow on deep documents

`Xml.text` and `Xml.elements` recursed once per nesting level
(`kids.map(text)`, `kids.flatMap(elements(_, name))`). `Xml.cst` built a
20 000-deep `<a>…</a>` document without trouble, and both projections
then threw StackOverflowError on it. This is the defect
`cst-walk-stack-safe` fixed in okay-parse; it was found while porting
Xml to okay2 (okay2-xml). Both walks now do a pre-order over an explicit
stack. TestXml has a new test at that depth, and it failed with
StackOverflowError before the fix.

A survey for other walks of the same shape found `Yaml.values` and
okay-rag's `Split`/`Symbols` walks. They are filed as
`cst-walks-remaining`.
