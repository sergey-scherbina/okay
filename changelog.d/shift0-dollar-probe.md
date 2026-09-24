## shift0-dollar-probe - stage 0: λ$'s `$` on today's Delim, and the typing question answered

TestDollarProbe (5), specs/shift0-dollar.md stage 0, no machine change.

- The ICFP 2011 example ("A cat has Alice.") runs on today's `shift0`,
  with one prompt pushed twice.
- APLAS 2012's macro-expression of `$` through reset0/shift0, written on
  today's `push`/`shift0`, obeys both λ$ rules. The obvious
  `push(p)(e).flatMap(v)` breaks `($/S0)`: it gives "<dropped>"
  instead of "dropped" and "<a!b!>" instead of "<a!><b!>". This was
  watched failing: with the macro's body swapped for the flatMap
  encoding, exactly those three tests went red.
- The spec's open question is answered by APLAS's `S k.e = S0 k.⟨e⟩`.
  An under-prompt capture's body runs under a PLAIN delimiter, and our
  machine already does this, so `Prompt[R]` needs no new kind.
- Consequence for delim-dollar: `$` is expressible today, and the
  primitive has to earn its place by price or typing.
