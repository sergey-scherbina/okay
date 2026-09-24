## affected-separate-builds - another build's .sbt is not this build changing

`scripts/gate.sh "affected master"` read ANY `*.sbt` as a change to the
build and ran the whole family. `okay2/` and `okay-ts-browser/` are
separate builds with their own `project/`, so every lane touching their
`build.sbt` paid for everything: ts-browser-facades, a docs-and-example
lane, ran 7108 tests. The same rule missed the opposite case: a source
change in an sbt plugin the meta-build compiles (`okay-deploy/sbt-plugin`)
IS a change to this build, and was not counted.

- project/Affected.scala `buildChanged`: the root `*.sbt`, the root
  `project/`, and the directories named by `RootProject(file(...))` in
  `project/*.sbt`. An `.sbt` anywhere else is another build's, and its
  files belong to no project here.
- Checked on real history with `affected <a>..<b> name`:
  - ts-browser-facades now selects 1 project (okayDeploy), not 174;
  - an okay2 build.sbt commit selects nothing (okay2 gates itself);
  - a root build.sbt commit still selects all 174;
  - a commit to okay-deploy's sbt-plugin sources now selects all 174,
    where before it selected only okayDeploy.
