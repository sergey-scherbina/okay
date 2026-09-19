# Bugs — okay-codec

Defects owned by this module. Status lives in the machine-readable
header, never in prose.

## stackbytes-json-read-not-flat-on-aarch64 — `Json.read[Tree]` costs 256 KB at depth 100 where the test wants 16
<!-- status: open
     lane: jvm
     area: okay-codec/src/main/scala/okay/codec/Json.scala (or the test's own probe)
     gate: okay-codec/src/test/scala-jvm/okay/codec/TestStackBytes.scala "both threshold lanes: every door is flat past the threshold"
     found-in: 6a045d2f
     confirmed: no -->

`TestStackBytes` asserts that past the trampoline threshold a door's
stack cost stops growing — `deep == shallow` — and on this host
`Json.read[Tree]` measures **16 KB at 8 levels and 256 KB at 100**.
The other doors in the same printout are flat (`JsonValue.parse`,
`Json.lossless`, `Json.readStrict[Tree]`, `Staged.strict[Tree]` all
16 KB; `Staged.cbor[Tree]` 64 KB, `Cbor.read[Tree]` 256 KB).

**Not caused by the lane that found it.** Measured 2026-09-19 twice:
in the `mcp-tool-authorization` worktree, and then on master itself
(`6a045d2f` — master plus a claim file, no library source differing),
where it fails with the identical numbers. Recorded so the next full
gate does not spend the finding again.

**What is NOT known, and must not be assumed:** whether this is the
library or the measurement. The host is aarch64 under a container,
JDK 21.0.10 (Ubuntu), and the repository's numbers were taken on the
operator's own machine — frame sizes and the probe's 16 KB
granularity are both arch-sensitive, so "the trampoline does not
cover this door" and "the probe cannot see it flatten here" are still
both live. The cheap discriminator first: run the same suite on the
operator's machine at this commit. If it is green there the entry is
about the test's portability, not about `Json.read`.
