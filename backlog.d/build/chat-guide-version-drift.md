- [ ] chat-guide-version-drift — docs/building-a-chat-app.md §1 tells
      the reader to depend on `0.1.0-SNAPSHOT` ("`ThisBuild / version`
      in okay's build.sbt"), but build.sbt says `0.1.1`, and a
      `publishLocal` today publishes `0.1.1` (seen 2026-09-23 by the
      scala2-docs lane). A reader following the page gets
      "not found: dev.okay#okay-jetty_3;0.1.0-SNAPSHOT", which is the page's
      own Troubleshooting entry, for a different reason. The page's
      contract is that every command was executed before it was
      written down, so the fix is to re-run §1 (publishLocal, the
      artifact count and time) and update the table from the run, not
      to edit the version string alone.
