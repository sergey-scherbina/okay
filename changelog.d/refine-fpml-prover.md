## refine-fpml-prover - okay-refine reads ISDA's public FpML examples end to end

- `Xml.value(cst): Json` and `Xml.attributes(lexeme)` in okay-codec
  (okay2 in step): the document as the one `Json` value every dialect
  projects into — elements as objects, attributes as `@name`, repeated
  children as arrays, text as strings, mixed text under `#text`, names
  as the token spells them; explicit stack. `Format.value` projects XML
  through it now (it declined XML before).
- `Refine.json.{field, str, num, each}` — the Json-level steps a
  document pattern is written in, each named, so a verdict's path reads
  `dataDocument/trade/swap` and a refusal names the missing element.
- The prover, in test scope (`okay.refine.fpml`): FpML 5.10 ird-ex01
  (vanilla EUR swap, 6% vs EUR-LIBOR-BBA 6M) and fx-ex03 (EUR/USD
  forward at 0.9175), verbatim from the FINOS CDM repository, read
  bytes → xml → value → FpML → swap | forward with the other branch's
  refusal named; `read(write(x)) == x`; the path's write comes out as
  JSON and reads back by the json branch; `Logic.ifte` over both. No
  ambiguity met, so the `Judge` seam stays deferred (specs/refine.md
  Results).
