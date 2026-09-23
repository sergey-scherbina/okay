- [ ] polyglot-remote-foreign — `okay.Foreign` over a WIRE, the piece
      every out-of-process language below shares (operator, 2026-09-23:
      "как мы можем с ними прозрачно работать?"). Today okay-py and
      okay-r are CALL-shaped: okay calls a named function and gets an
      answer, but Python/R code cannot PERFORM an okay operation (ask a
      Reader, write the journal, call another handler) mid-call. The
      in-JVM bridges already can (interop-shared's `Foreign.View`: Done /
      Await / Tell / Perform / Lift). Across a process: each step is a
      CBOR message `{kind, payload, k: id}`, okay answers
      `{resume: id, answer}`, and the far side keeps its continuations in
      a table by id. Where the far side's continuation is a PURE function
      (Haskell, a Frege-style freer tree) the same id can be resumed twice
      — multi-shot survives the wire; where it is a generator (Python,
      JS) it is one-shot and a second resume is refused by name, the
      rule `Gather.stage` already keeps. Spec first (specs/polyglot.md);
      the shim protocol is versioned like okay-py's handshake.
