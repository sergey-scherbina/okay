- [ ] foreign-managed-env — the environment declared in code:
      `PyEnv(python = "3.12", packages = Map("numpy" -> ">=2,<3"))`.
      okay builds it with `uv` into a cache directory keyed by the lock
      file's hash (R: `renv`), so a fresh machine and CI get the same
      interpreter without a README step. `verify` (which exists) stays
      the check that the environment is right; this is what makes it
      right in the first place. The worker's process gets that env and
      nothing else, as the clean environment already works.
