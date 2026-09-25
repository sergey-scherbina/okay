- [ ] foreign-one-modules — stage 5 of specs/foreign-one.md: the facade
      filled, and "add a language" as a procedure. Module types
      `TsModule`, `HsModule`, `GoModule`, `RustModule` (a name plus what
      the language needs to find the code) with the instances each
      honestly gives; `CljModule`/`FregeModule` over their own
      `Foreign.View` with `Programs` instances whose `Op` is empty —
      foreign-facade's Decision 7 (no JVM instance) narrows to
      `JvmModule`, the Scala-function case (spec Decision 7). The
      protocol as ONE golden transcript (`specs/foreign-wire.txt`: every
      conformance case's messages, host and far side labelled) replayed
      against a fake far side in the default gate and against every shim
      live, so a new language is written against the transcript, not by
      reading shim.py. docs/foreign-facade.md "Adding a language" becomes
      the four-step checklist (library on a link, module type +
      instances, `ops` writer, three conformance suites green). Gate:
      `FacadeConformance` green per claimed instance; a claimed-but-absent
      instance fails `summon` under `compileErrors`; the transcript
      replays.
