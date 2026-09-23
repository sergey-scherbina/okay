## generators-docs - Gen in the guide, the textbook and the typepedia

The generators lane (2026-09-22) shipped direct-style.md and tutorial
ch. 2 only; the operator's rule is the four doors. Now docs/guide.md
§3 has `Gen[W]` beside `FoldUntil` (the three ways to write one, the
three endings, the Python law of `iterator`), theory ch. 7 has the
paragraph that fixes the words — PEP 255 for the everyday semantics,
James & Sabry 2011 for `yield` AS a delimited continuation, and why
that is the implementation here (the held `k`, applied on the NEXT
`next()`; the eager first cut) — with chapter 2's tree walk written as
a `Gen`, and the typepedia has the `Gen[W]` entry. Every snippet is
verbatim in `TestDocExamplesGen` (okay-direct): a `match` with marks
in its cases inside a `generator` block, and a counter proving the
walk holds after the first leaf.
