## cont-machine-resume - one `resume` for the machine: no throwaway `Cat` per resumption

The operator's ask (2026-10-01): look again at what can be simplified.
`Rev.onto` put a resumed `k` over the live stack as `Cat(k, live)`, and
`pushed` took that `Cat` apart at once into a second one. Now one
`resume(focus, k, fs, st)` builds the registers directly: `k`'s head
segment into the frames register, the rest of `k` catenated over the
live stack (or `k` itself over nothing). The machine's start goes
through it; `Rev.onto` and `pushed` are gone, `Rev` is only the general
cut's prefix. Against master: 0.96-1.04x on six resumption lanes,
bytes down on stateDeep (-48 KB/op) and contAnswer (-24 KB/op), equal
elsewhere (the JIT had removed the throwaway on those). Stale comments
(Rev's header, a `Reset0` in Delim) restated.
