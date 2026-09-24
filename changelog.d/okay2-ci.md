## okay2-ci - CI runs the okay2 build

okay2 is a separate sbt build, and until now nothing ran it but a lane
that touched `okay2/`: `affected` and `family` see only the root build.
`.github/workflows/ci.yml` gains an `okay2` job — `cd okay2 &&
../scripts/gate.sh test` on every push or pull request that touches
`okay2/` or the gate script, and every night; it passes only on the
`gate: GREEN` verdict line. JDK 21, measured cold before landing
(336 results GREEN; the local gate had only run on 25).

Spec: specs/okay2.md "CI"; docs/okay2.md section 1.
