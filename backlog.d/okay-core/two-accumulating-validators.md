- [ ] two-accumulating-validators — `okay-codec`'s `Validate.gather`
      is `Validated.app` written by hand on `Either`, and its own
      comment says so: "the applicative step: both sides' errors
      survive". Two answers to one question now live in this
      repository (found 2026-09-18 while checking whether okay-ui's
      applicative traversals work with `Validated` — they do, freely,
      and TestUiApplicative pins it). NOT a rewrite request:
      `Validate` works, is tested, and sits in the schema hot path.
      What is worth doing is a BRIDGE, so a form's errors can be read
      either way, and a line in each file pointing at the other.
      Trigger: the first consumer that wants a schema walk's errors
      inside a `direct` block, or the second time someone asks which
      of the two to use.
