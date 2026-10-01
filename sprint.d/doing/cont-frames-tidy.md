- [ ] cont-frames-tidy — three simplifications of the segmented frame
      machine (Cont.scala), no behaviour change: (1) unpacking a `Kept`
      (its delimiter as a `Reset` over `End`/`Done`) is spelled three
      times — `cut`, `pushed`, the loop's `Return` arm — one method on
      `Kept`; (2) the bare cut's `rebase(...).asInstanceOf[...]` is a
      second claim (a plain delimiter's body answers the prompt's type)
      written inline — one named function, its argument beside it;
      (3) `Frames.as`, `Frames.resume`, `Frames.runOf` are public and used
      only by the machine and `Rev` — narrowed. Gate: the machine's 24
      suites + `affected master Test/compile` (no behaviour change);
      one A/B lane (delimGenerator) to confirm nothing moved. Then
      backlog cont-frames-head-form-run (profile writerTellUnderDelim).
