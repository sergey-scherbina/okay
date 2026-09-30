- [ ] indexed-effects-4-delim-signature — stage 4 of
      specs/indexed-effects.md, a spike with a landing: Delim's
      operations carrying the prompt stack as the index they move
      (`Push` installs `p.type`, a 0-capture pops it), payloads typed
      where the signature can say it instead of `Any`, `Delim.Stacked`
      re-expressed as that signature's doors rather than a facade over
      `Prog`. The machine's `Segs` stays. What the compiler refuses is
      recorded in the spec's Decisions; TestDelim and TestProg green
      unchanged. Changes existing behaviour if the machine's casts
      move: the full `affected master staged` then.
