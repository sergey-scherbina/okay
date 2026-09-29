## zio-typed-row — the whole ZIO[R, E, A] as an okay row, both ways

`ZioInterop.fromZIOTyped` / `toZIOTyped` cross a ZIO with an environment
and a typed error: `ZioRow[R, E] = Reader % ZEnvironment[R] + Throws % E +
Async`. The environment is the Reader (`Reader.run(env)` provides it on
the okay side, `provideEnvironment` on ZIO's), a typed failure is
`raise(e)` / `fail(e)`, a defect stays a defect (an okay throwable becomes
`die`, never `E`), and cancellation interrupts the fiber as in `fromZIO`.
The round trip answers as the original ZIO. Mutant-checked.

Found: `.at[ZioRow[G, E]]` finds no row membership even at concrete
arguments; the same row spelled out does. The bridge widens by name, and
the docs say to spell the row for `.at`.

Spec: specs/zio-typed-row.md. Docs: docs/modules/okay-zio.md.
