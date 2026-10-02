## delimited-machine — the machine apart: Delimited.scala is the whole machine

Operator decision, 2026-10-02 (design conversation, sprint cont-js-depth
stage 1). `Frames`, `Stack`, the loop (`Frames.run`/`machine`/`enterAt`),
`Cont0 = Shift0 | Reset0` with its delimiters and `Rev` moved verbatim
from Cont.scala to Delimited.scala, beside the DPJS interface: the
machine — continuations as data, answer-type indexes, multi-shot — is one
file. Cont.scala is the one-prompt facade and the strict-`k` BRIDGE (a
continuation as a host function), the one place a run nests the host
stack. No behaviour changed: 354 compiles, core's 722 tests green. The
census of opaque `shift` bodies (76 sites; only state passing through a
function answer actually nests) is in specs/cont-js-depth.md.
