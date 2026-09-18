- [x] blob-byte-source — DONE 2026-09-16 (blob-source-road):
      `Bytes.file`, `Bytes.stream`, `Bytes.fileSource` and `putFile`
      on the jvm; `Backup.stream` is now one line. Was: a byte stream from a `Path` or an
      `InputStream`, in the library. The 64 KB read loop that
      `Backup.stream` has (private, `okay-blob/.../Backup.scala:65`)
      now exists a SECOND time, copied verbatim into okay-watch,
      because there was nothing public to call. Anyone else putting a
      file into a Blob writes it a third. Cheapest of the four, no
      breakage, and it removes the hand-written `effect` from every
      caller — which is where the defect above lives.
