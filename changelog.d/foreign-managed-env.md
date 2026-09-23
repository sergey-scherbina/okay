## foreign-managed-env - Python and R environments declared in code

Stage 8 of specs/foreign-highlevel.md, and the last of the series.

- `PyEnv(python, packages)` builds a `uv` venv into a cache directory
  keyed by the declaration. `start()` provisions it, starts a worker on
  it, and verifies the packages.
- `REnv(packages)` fills a keyed CRAN library with a fixed provisioning
  script shipped in the jar, and starts a session that requires the
  packages.
- A build that died is rebuilt, not trusted.
- Concurrent provisions take a file lock.
- An unknown package refuses with the tool's own words.

Tests: 5 live (a pinned `six` through uv, `praise` from CRAN inside the
docker R) and 3 key tests in the default gate. A mutant is caught.
Docs: "The environment/library, declared in code" in
docs/modules/okay-py.md and docs/modules/okay-r.md. The spec records
the whole series as closed.
