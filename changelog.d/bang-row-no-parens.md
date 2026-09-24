## bang-row-no-parens - `A ! State % S + Writer % W`: the parentheses after `!` are gone from Scala 3 okay

- In Scala 3 an infix type operator's precedence comes from its first
  character (Effects.scala:87 already said so): `!` sits at the `= !`
  level, `+` and `%` above it, so `A ! (State % S + Writer % W)` and
  `A ! State % S + Writer % W` are the same type — measured, and pinned
  by `TestRowSpelling` (`=:=` three ways, and a program written at the
  bare row runs). docs/direct-style.md alone wrote it bare; 682 sites
  in 199 files (sources, tests, benchmarks, docs) wrote the parentheses.
  All stripped, by a script that touched only a `%`/`+` chain of names
  followed by nothing that binds tighter than `!`.
- The Scala 2 side is the ONE place the parentheses stay, and it is
  now the one visible difference between the two spellings: 2.13 gives
  every infix type operator the same precedence (stage 17), so
  `A ! (State[Int] + Writer[String])` keeps them. docs/scala2.md's
  section 3 and its comparison table (section 9) say so side by side;
  the Scala 2 rows in typepedia, guide, theory ch. 13, jvm-languages
  and your-own-effect were skipped by the script and checked by hand.
- 28 example lines that `docs/snippet-debt.txt` holds verbatim (the
  pseudo-signatures no test can pin, `def op[F[+_]](...)`) keep their
  parentheses: the ratchet lets the debt only shrink, so an edited
  debt line would be a new unpinned line. They lose the parentheses
  the day each is pinned.
