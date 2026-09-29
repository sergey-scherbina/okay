## groupid-io-github - the organization is io.github.sergey-scherbina, so a release can reach Maven Central without a domain

The operator's call (2026-09-29): "Делай пока везде io.github.sergey-scherbina -
потом будем думать про dev.okay". `dev.okay` needs proof of owning
`okay.dev` before Central accepts it; an `io.github.<user>` namespace is
verified through the GitHub account alone. So 0.2.0 is publishable now,
and the domain question is decided later (specs/modules-infra.md,
"Publishing", says why before 0.2.0 is the cheap moment to move again).

- `organization` in the root build, okay2's build, okay-ts-browser and both
  sbt plugins (okay-deploy, okay-frege).
- Every doc that shows a dependency line or an `~/.ivy2/local/<org>` path,
  and the matching `docs/snippet-debt.txt` entries (the same lines,
  re-spelled; the ratchet did not grow). The five-way harness patch
  depends on the new coordinates.
- ROADMAP, AGENTS.md and the spec record the decision and its history.
- `okayJVM/publishLocal` surfaced two scaladoc warnings on master
  (`$s` in Chronicle's example, `${ws.size}` in `Writer.censor`'s):
  escaped. publishLocal now green with no warnings, and it delivers
  `io.github.sergey-scherbina#okay_3;0.2.0-SNAPSHOT`.
