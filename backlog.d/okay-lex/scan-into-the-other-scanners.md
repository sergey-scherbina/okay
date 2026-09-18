- [ ] scan-into-the-other-scanners — `Yaml`, `Markdown`, `Xml` and
      okay-rag's `Code` scanner still answer the pair, so they still
      pay the `Tuple2` per character and a `Vector` per token; the
      default `stepInto` keeps them correct, not fast. Json's move
      says the shape of the win (~29% of the allocation on that
      scanner's road), but none of the four has a measured lane, and
      `Code`/`Yaml` recurse into their own `step` — the conversion is
      real work, not a rename. Wants a lane that measures one of them
      first: okay-rag's code chunker over a real file is the honest
      workload.
