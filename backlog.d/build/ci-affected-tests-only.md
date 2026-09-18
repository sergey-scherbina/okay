- [ ] ci-affected-tests-only — a lane that changed only a module's
      TESTS re-tests its dependents too, which is conservative and
      wrong; the closure should skip dependents when nothing under
      `src/main` moved. Cheap once the numbers say it matters.
