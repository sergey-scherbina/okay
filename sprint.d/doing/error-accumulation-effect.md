- [ ] error-accumulation-effect — PRIORITY: LOW (design). Found by the
      core review 2026-09-26. `Validated` (specs/validated.md) accumulates
      errors as APPLICATIVE data: independent checks, all errors
      reported, no binds between them. There is no EFFECT for the case
      in between Throws (stop at the first error) and Validated (no
      dependencies allowed): a program that RECORDS a non-fatal error and
      goes on, possibly with a fallback value, and fails at the end if
      anything was recorded. Names in the literature: `MonadChronicle` /
      `ChronicleT` (Haskell `these`), `Ior`/`IorT` (cats), arrow-kt
      `Raise.accumulate` / `mapOrAccumulate`, zio-prelude
      `ZValidation` (with warnings). Shape here would be one signature
      with two operations, `dictate(e)` (record and go on) and
      `confess(e)` (record and stop), and a handler answering
      `Ior[NonEmpty[E], A]` (errors only, both, or value only). It is
      close to `Writer % E` + `Throws % E` run together, and the lane
      should first check whether that composition is enough and only
      then add a signature. Consumers: okay-conf (warnings about
      deprecated keys next to a valid config), okay-openapi (a body with
      unknown fields accepted but reported).
