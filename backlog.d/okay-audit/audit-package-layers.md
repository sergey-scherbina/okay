- [ ] audit-package-layers — stage 1 of specs/audit-ready.md: `auditLayer`
      per sbt project is too coarse for a product that is one project by
      design (okay-watch: `okaywatch.trace` is business, `okaywatch.collect`
      and `okaywatch.api` are handlers, all in one jar). Add
      `auditLayers: Map[String, String]` (package prefix → layer); a class
      is classified by the longest matching prefix, the project's layer is
      the default; the manifest gains a column and `Audit.run` a match on
      `Ref.from`. Done-when: the dogfood can state okay-watch's packages and
      TestAudit has a two-prefix fixture.
