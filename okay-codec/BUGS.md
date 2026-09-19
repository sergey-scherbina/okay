# Bugs — okay-codec

Defects owned by this module. Status lives in the machine-readable
header, never in prose.

## stackbytes-probe-measures-the-hosts-floor — why a container read 256 KB, answered
<!-- status: fixed
     lane: jvm
     area: okay-codec/src/test/scala-jvm/okay/codec/TestStackBytes.scala
     gate: same file, "both threshold lanes: every door is flat past the threshold"
     fixed-in: this lane
     confirmed: yes -->

The open half of stackbytes-json-read-not-flat-on-aarch64 (closed
wontfix, correctly — no main source was ever at fault): WHY the
container measured 256 KB. Three causes, all measured here, all in
the MEASUREMENT.

**1. A thread's stackSize is a request, and this host floors it.**
`new Thread(g, r, name, stackSize)` reserves a guard zone inside what
you ask for — `StackShadowPages`, plus red and reserved pages — and
floors anything smaller than what is left. aarch64 Linux in a
container, JDK 21.0.10, 4 KB pages, `StackShadowPages=20`:

```
requested   8  16  24  32  48  64  96 128 KB -> 876 frames, every one
requested 160 192 224 256 KB                 -> 1490 2308 3128 3948
```

Eight requests, ONE stack of ~34 KB usable: 80 KB of shadow inside a
~128 KB minimum. A ladder of powers of two has one rung below 256
there, so a door needing a little over the floor reports 256 — an 8x
overstatement — and a door sitting ON the floor flaps between rungs.

**2. The cold round answers a different question.** An interpreted
frame is several times a compiled one. Cold, depth 8 answered
`256, 16, 16, 16, 16` across five rounds; warm, depths 8, 100, 200
and 400 answered 16 every round. The file's max-of-3 is right for
"what can this door ever need" and wrong for "does the trampoline
engage".

**3. And the comparison itself was wrong.** It asked whether 8 levels
and 100 levels cost the same — but 8 is BELOW `Codecs.NativeThreshold`
and 100 is past it, so it compared the native path against the
trampolined one. Different code, and the native one can legitimately
cost more: measured here, `Cbor.read[Tree]` wants 256 KB at 8 levels
and 16 KB at 100 and at 400. The old assertion called that a failure
to flatten; it is the trampoline working.

Fixed by asking the promise the two trampoline lanes actually made —
past the threshold the cost stops following the depth — with both
compared depths past it (`levels` and `levels * 4`), on warm doors,
plus `deep <= shallow` so the trampoline may never cost more than the
recursion it replaced. 216 tests green here, and the printout now
reads `16 KB at 8 levels, 16 KB at 100, 16 KB at 400` for
`Json.read[Tree]`.

## stackbytes-json-read-not-flat-on-aarch64 — the container's measurement, not `Json.read`
<!-- status: wontfix
     lane: jvm
     area: TestStackBytes.scala's probe, or the container's JVM/host, not okay-codec's main sources
     gate: okay-codec/src/test/scala-jvm/okay/codec/TestStackBytes.scala "both threshold lanes: every door is flat past the threshold"
     found-in: 6a045d2f
     confirmed: yes -->

**The entry's own discriminator answered it, 2026-09-19.** Run on the
operator's own machine (also aarch64 — Apple Silicon arm64, JDK
21.0.12), at a commit after this entry: `TestStackBytes` is GREEN,
and `Json.read[Tree]` measures **16 KB at both depth 8 and depth
100** — flat, not 256 KB. All three tests in the suite pass. This is
exactly the outcome the entry said would settle it: "if it is green
there the entry is about the test's portability, not about
`Json.read`." It is green here, so it is.

Nothing in `okay-codec`'s main sources is at fault; no code changed.
What is still genuinely unknown, and out of this entry's scope: WHY
the container's aarch64-under-container JVM (JDK 21.0.10, Ubuntu)
measured 256 KB at all — a JVM/host difference the probe's 16 KB
granularity cannot itself explain, and nobody has reproduced on that
specific container since. If it recurs there, open a fresh entry
naming the container image and JVM build; this one is closed.

Investigating it did find one real thing, elsewhere: `decodeC-ssum-
defer` (changelog.d/decode-c-ssum-defer.md) — `SSum`'s case in both
`Json.decodeC` and `Cbor.getC` called its own recursion directly
instead of through `Cont.defer`, the one recursive branch in either
fold that didn't. Not the cause of THIS entry (`Tree` has no sum
type; `Schema.derived` always wraps a case's payload in a product,
whose own field IS deferred, so no `derives`-built schema could ever
observe the gap either) — found and fixed on its own merits, with a
hand-built `Schema.SSum` that reaches it directly.

### Original report (superseded 2026-09-19)

`TestStackBytes` asserted that past the trampoline threshold a door's
stack cost stopped growing — `deep == shallow` — and on the
container host `Json.read[Tree]` measured 16 KB at 8 levels and
256 KB at 100. The other doors in the same printout were flat
(`JsonValue.parse`, `Json.lossless`, `Json.readStrict[Tree]`,
`Staged.strict[Tree]` all 16 KB; `Staged.cbor[Tree]` 64 KB,
`Cbor.read[Tree]` 256 KB).

Not caused by the lane that found it: measured twice, in the
`mcp-tool-authorization` worktree and then on master itself
(`6a045d2f` — master plus a claim file, no library source differing),
identical numbers both times. What was not known: whether this was
the library or the measurement — the host was aarch64 under a
container, JDK 21.0.10 (Ubuntu), while the repository's own numbers
were taken on the operator's machine, and frame sizes plus the
probe's 16 KB granularity are both arch-sensitive.
