## audit-cli-standalone — run the boundary audit from Maven or Gradle without SBT

`okay.audit.Main --manifest audit.json --report build/audit` accepts a strict,
zero-dependency JSON manifest of layers, class directories/jars, package
exceptions and named allows. It writes the established text and JSON reports
and returns 0 for a pass, 1 for findings and 2 for invalid input. Relative
input paths resolve beside the manifest; the legacy TSV entry point remains
available to SBT.

Spec: `specs/audit-cli-standalone.md`. Test: `okay.audit.TestAudit`.
