- [~] blob-create-only — bounded immutable object creation for cloud financial commits

Add a bounded immutable-object facade for cloud commit markers and chunks.
S3 signs If-None-Match:* and distinguishes 412 from transient 409 and errors.
No unconditional fallback. Read limit enforced while consuming the body.
Spec: specs/blob-create-only.md. Gate new okayBlobJVM suite and affected compile.
