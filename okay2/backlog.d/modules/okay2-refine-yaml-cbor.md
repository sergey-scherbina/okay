- [ ] okay2-refine-yaml-cbor: okay2-refine's format level reads JSON and
      XML and says "YAML and CBOR join the level the day okay2-codec reads
      them" (okay2-refine/src/main/scala/okay2/refine/Format.scala,
      okay2-refine/README.md, the okay2Refine comment in okay2/build.sbt,
      docs/okay2.md section 31). okay2-codec reads both since
      okay2-codec-cbor and okay2-codec-text (2026-10-04): add them to
      `Format.detect`/`Format.value` as okay-refine has them, and correct
      those four sentences. (2026-10-04)
