- [ ] direct-for-over-source — `for x <- src do body` in a direct
      block where `src` is an EFFECTFUL source, not a collection: a
      `Gen[W]` (okay-direct's generator view over `Unit ! Writer % W`,
      direct-staged-v2), a `Stream[S, F]` instance, or the `Take`
      side of a `Stage`. direct-loops (specs/direct-loops.md)
      iterates a materialised `List` only, for multi-shot safety, so
      today the consumer of a generator is `foldUntil`'s combinators
      or a hand-written `!.loop` over `uncons` — the iteratee is here
      (Pipe.scala: `Take.Await` + `Writer` + `pipe` is Kiselyov's
      pairing by delimited continuation, `Stage` the enumeratee) and
      what is missing is only the loop that reads as a loop. The
      macro emits `!.loop(src) { s => uncons(s) match … }` — the
      fold-until stage-2 primitive — with the body's marks handled as
      in the `while` road, and the multi-shot argument is answered
      differently from the List road: a `Writer` program is
      re-runnable data, so re-entering the rest of the loop per
      element re-steps the program, and a live `Iterator` source is
      REFUSED with the v1 message rather than materialised. Inside a
      `direct[Take % I + Writer % O]` block the same form is
      `for i <- input do tell(f(i)).!?`, a `Stage` written straight.
      Distinct from direct-loops-v2 (claimed 2026-09-22), which adds
      collection HOFs and multi-generator `for` over `List`; this is
      the source side. TRIGGER: `Gen` landed from direct-staged-v2 and
      one consumer written against it by hand — the fold-until spec
      names `take/first/exists` through `Writer.foldUntil`; the first
      body that is neither is the trigger.
