- [ ] foreign-one-docs — stage 8 of specs/foreign-one.md: ONE entry page,
      "Foreign languages" — the five things that cross (a value, a table,
      an object, a stream, a program), the two runtimes, the markers as
      the table of who carries what, one program written in every
      language and the SAME Scala over every runtime, the wire's five
      operations and the transcript, adding a language in four steps.
      docs/python-and-r.md, docs/rust.md, docs/one-language.md,
      docs/foreign-facade.md become its per-language and per-layer
      chapters (their Scala examples move with them, every line still
      pinned by `TestDocSnippets`; `docs/snippet-debt.txt` only shrinks).
      Literature carried from foreign-facade (Truffle interop, Wadler &
      Blott, Arrow Flight, Jupyter kernels, Erlang ports) plus credit-based
      flow control (Reactive Streams §3, RFC 9113 §5.2) and the Arrow C
      Data Interface. Gate: `TestDocSnippets` and the doc-example suites.
