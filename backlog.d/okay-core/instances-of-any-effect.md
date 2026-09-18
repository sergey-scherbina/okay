- [x] instances-of-any-effect — DONE (2026-09-11): `Instances[F]` is
      `Tag` with the key read at RUN time — one row member per
      signature, however many instances, identity by a fresh `Handle`
      compared by reference. `at`/`route` to perform and to send an
      already-written program to an instance, `handler(pick)` to run
      them all in one pass, `only(h)` to strip one back to the plain
      signature for the effect's own runner (the others stay in the
      row), `exhausted` to assert none survived. No cast: the handle
      is compared by reference and the operation is already typed.
      `TestInstances` pins instances made IN A LOOP, which is the
      case neither `Tag` nor `Refs` could serve.
