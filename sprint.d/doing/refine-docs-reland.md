- [ ] refine-docs-reland — re-land a19b67d6f (docs/modules/okay-refine.md:
      writing a document level, the corpus method, the cross-format law,
      what lives where), which ci-runner reverted as 63c5ffe87 after a
      bisect on `okay.agent.TestFleet` ("waited 5000ms", a timing flake:
      green alone on the reverted master, 2026-09-29 15:40) converged on
      a DOCS-ONLY commit and the confirmation step counted a
      warnings-RED (the TestBot E176s, since fixed) as reproduction. The
      runner defect is backlog `ci-runner-docs-only-culprit`. (2026-09-29)
