## delim-internal-shift0 - `emit`, `onReturn` and `exit` capture with `shift0`: collect 0.77x

The operator's ask (2026-10-01), the cheap half of the removed `under`
flag's price: `shift` is `shift0` whose body runs under a fresh `reset`
of the same prompt, and the two differ only when the body captures to
that prompt AGAIN. The library's own pattern doors never do — `emit`'s
`onEmit` (`Emitting` is sealed: `Listing`, `Stopping`), `onReturn`'s
`k(()).map(f)`, `exit`'s dropped `k` — so they now capture with
`shift0`, and the `Inject`, `Dollar0`, closure and loop step a `reset`
cost per capture are gone. `Delim.shift` for users is unchanged.

- New lane `CollectBenchmark.collectEmit` (okay-direct): `Delim.collect`
  over 1 000 `emit`s in a direct block — 0.77x master, 502 vs 606 KB/op
  (~104 B less a capture). DelimBenchmark.delimGenerator spells its emit
  with a raw `Delim.shift` and is unaffected.
- `Control` and `Delimited` state how they stand: one prompt and the
  user's level with a closure instance, against the machine's interface
  that `Control[Cont]` is built on.
