- [ ] okay2-ci — okay2 is a SEPARATE sbt build, and nothing runs it but a
      lane that touches `okay2/`: `.github/workflows/ci.yml` does not
      mention it, the nightly does not, and `affected master` cannot see
      a directory outside the root build (checked 2026-09-24). Its 186+
      tests go stale the day nobody's lane touches it. Lane: a CI job
      (and the nightly) running `cd okay2 && ../scripts/gate.sh test`.
      Note the gate's warning check reads Scala 3's `[warn] -- [Exxx]`
      format; okay2 relies on `-Werror` instead, which is enough.
