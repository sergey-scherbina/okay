- [ ] root-aggregate-unlisted-modules — seven modules are defined in
      build.sbt but absent from the root `.aggregate(...)`, with no comment
      saying why: okayOps (crossProject), okayDocsDynamo, okayDocsCassandra,
      okaySpring, okayGuice, okayCdi, okayOpenapi. Being absent means the
      full matrix (`scripts/gate.sh`, the nightly) never runs their tests
      and `publishLocal` never publishes them. None is a compile dependency
      of a published module: checked 2026-09-23 by scanning every
      `~/.ivy2/local/dev.okay/*/0.1.1/ivys/ivy.xml` for non-test dev.okay
      dependencies that were not published. That scan is how okay-js and
      okay-acme were found and added (pom-jmh-and-chat-version). Four others
      are out DELIBERATELY and say so beside their definitions
      (langchain4j-embed, onnx, demo-e2e-browser, ui-gtk). For each of the
      seven: add it, or write the reason beside it the way those four do.
      Some probably need docker or a live service; check whether their
      suites are Live-tagged before adding them.
