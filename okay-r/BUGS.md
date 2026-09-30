# Bugs — okay-r

The ledger for defects whose FIX lands in this module's sources
(`okay-r/src`). The root `BUGS.md` explains the ledger; newest first;
status lives in the machine-readable header, never in the prose.

## r-docker-road-private — the only road to R without an installed `Rscript` is test-private, so the first consumer copied it
<!-- status: open
     lane: okay-r (RSubprocess.scala; TestR.scala has the code today)
     area: sessions / where the interpreter is
     found-by: okay-fin fin-models (2026-09-29), the first module outside
       this repository to call REval (docs/modules/okay-r.md still says
       "nothing here has a consumer yet")
     repro: a JVM program on a box with docker and no Rscript on the PATH
       calls RSubprocess.start("Rscript", …) — IOException, the process
       does not start; TestR.container(image, packages) would have
       reached R through r-base:4.4.1 + r-cran-jsonlite, but it is
       `private[r]` in src/test and unreachable from a dependant
     gate: a public RSubprocess.container(...) (or RSubprocess.find)
       answering the same shim TestR builds, with TestR calling IT —
       one road, in main, and okay-fin's Sessions.rscript deleting its copy
     workaround: okay-fin models/src/main/scala/okay/fin/models/Models.scala
       `Sessions.dockerShim` — 25 lines copied from TestR.container
       (image lookup, the Dockerfile, the env-forwarding shim mounted at
       java.io.tmpdir) -->

`TestR.container` is the road every okay-r test takes to a real R when
none is installed: docker `r-base:4.4.1` plus `r-cran-jsonlite`, an
`Rscript` shim at the same absolute path inside and out, the parent's
environment forwarded so the clean-env tests measure R rather than
docker. It is the ONLY such road, and it lives in `src/test` as
`private[r]`. A consumer meets exactly the situation the tests meet —
a developer's Mac with docker and no R — and has nothing to call: the
choice is `brew install r` (a 20-dependency toolchain install for a
demo) or a copy of the shim. okay-fin copied it (its `Sessions`
object), which is the defect: the second copy drifts the day the first
one learns something (the arrow image, a new `r-base`), and neither
copy is where a consumer would look.

The fix is the shape okay-py already has for Python environments
(`PyEnv.provision()` answers an interpreter): a public
`RSubprocess.container(image, packages)` — or `REnv.docker(...)` beside
`REnv(packages)` — answering the `Rscript` string `start` takes, with
`TestR` calling it instead of owning it. Not a new API for a caller:
the same `start(rscript, …)` with one more way to get `rscript`.

A second, smaller thing found on the same day, filed here rather than
lost: `docs/modules/okay-r.md` "Where the road goes" says no module
calls `REval`. okay-fin's `models` does (`Models.r`, `Models.rAsking`,
`Models.rPortfolio`; okay-fin/specs/models.md), and the doc line is
now false.
