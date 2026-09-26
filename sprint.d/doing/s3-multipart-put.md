- [ ] s3-multipart-put — okay-blob's S3 puts in parts, in constant
      memory (part of engine-object-store-io, 2026-09-26). `S3.put` drains
      the whole body into memory to sign it; an object larger than the
      heap cannot be written. Needs: S3 multipart upload (initiate, part,
      complete, abort) over the same SigV4, a `putParts` that pulls the
      source and holds one part at a time, and an abort on failure so no
      half object is ever visible. Gate: a Live MinIO object several times
      the part size written with one part held, read back byte for byte;
      a put that fails mid-way leaves no object and no upload open.
