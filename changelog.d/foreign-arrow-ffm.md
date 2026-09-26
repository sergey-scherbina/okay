## foreign-arrow-ffm — a table into in-process Rust as the Arrow C Data Interface (2026-09-26)

Measured first: a 1M-row table call into in-process Rust took 119 ms and
was almost all JSON codec. Now a Rust library loaded through FFM exports
`okay_exchange_table`, and a table call hands it the table as two C Data
structs read in place, the head alone a message; the answer's table comes
back the same way (released by Rust's callback), and one C Data cannot
carry comes back as a message. 20.7 ms against 119.8 ms, 5.8x
(MeasureRustTable). The Rust side is hand-written (no `arrow` crate in
every worker build); the JVM side is `CDataCodec` — `OkayCData`, ours over
FFM, the default, and `ApacheCData` over the optional `arrow-c-data`, one
import away — each proven to read the other's structs (TestCData). Nothing
changes for the caller: `ForeignWorker.inProcess` finds the export and
`wire` says `json/none+cdata`. Docs: docs/one-language.md, "In this
process, a table is not text at all". specs/foreign-one.md Decision 21.
