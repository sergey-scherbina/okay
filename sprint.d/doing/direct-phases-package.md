- [ ] **direct-phases-package** — the nine `Direct*` phase files of
      direct-compiler-phases (01993eb4) sit flat in `package okay`,
      `private[okay]`: machinery every file of the core sees beside
      the facade. Move them to `okay.macros` (src/main/scala/macros/);
      `okay.direct`, the obvious name, is REFUTED by a probe compile —
      E049 ambiguous `direct` under `import okay.*` + `import
      okay.Direct.*`, the shape seven files here use. Direct.scala
      and DirectProbe.scala are the only two referrers. DONE WHEN:
      cold gate green, spec Decisions corrected (it said "shadowing").
