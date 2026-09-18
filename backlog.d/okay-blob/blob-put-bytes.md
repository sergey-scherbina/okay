- [x] blob-put-bytes — DONE 2026-09-16 (blob-source-road):
      `putBytes`, `putChunk`, `getBytes`, concrete on the trait, and
      `Producer.each` in core for the walk that keeps the answer. Was: `put` over what callers actually hold: an
      `Array[Byte]`, a `Chunk[Byte]`, a `Path`. Today storing a file
      requires learning the Produce algebra first, and the streaming
      form is the only form. Independent of the above and smaller;
      together they would have made the defect unwritable without
      touching the trait.
