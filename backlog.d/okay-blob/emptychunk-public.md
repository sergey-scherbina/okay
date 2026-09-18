- [x] emptychunk-public — DONE 2026-09-16 (producer-drains): public,
      with its doc saying what it is for. Was: `Chunks.emptyChunk` is `private[okay]`, so
      a consumer writing a byte producer's terminator by hand, or
      passing `Source.toProducer`'s `end` for chunks, spells
      `ArraySeq.empty[Byte]` and hopes it is the same thing (it is —
      an empty `ArraySeq` is what `emptyChunk` casts to). Either make
      it public or give `Bytes` an `empty`; `putSource` already hides
      it for the common case.
