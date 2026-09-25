- [ ] cont-stack-docs — specs/cont-stack.md stage 7, with or right
      after cont-stack-switch (operator rule: a lane ships user docs
      with examples and literature): a docs/ page on `Cont`'s stack
      behaviour per platform. What a user must know: a program of
      transparent bodies (tail `k(v)`, `k(1) + k(10)`, `xs.map(k)`)
      never touches the stack; an OPAQUE body (a `k` handed to Java, to
      an abstract method, to a body passed as a value) runs direct and
      is switched to a fresh 1 GB thread when the room runs out — 33 µs
      a switch, then ~0.01 µs a level; how much room the caller's
      stack is granted on each platform (Native exact; JVM exact with
      `--enable-native-access=ALL-UNNAMED`, which the page says HOW
      to set and WHY — one avoided switch is more than a whole
      statePara run; JVM counted from `ThreadStackSize` otherwise, and
      the two written bounds: a thread with an explicit SMALLER stack
      sets `-Dokay.cont.room`, and a single opaque frame over 64 KB);
      JS: nested opaque bodies are bounded by the engine's stack
      (~10 800 frames on V8's default, `node --stack-size`). Every
      Scala example pinned in a `TestDocExamples*` suite
      (doc-snippets-pin-all), run `OKAY_SNIPPET_DEBT=write …` on the
      docs lane. Literature: Danvy & Filinski 1990; Rompf, Maier &
      Odersky 2009; Pettyjohn et al. 2005; JEP 444 / JEP 238 / JEP 472.
