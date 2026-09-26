# okay-blob

The object-store seam (specs/blob.md): bytes and streams in the
engine, meaning at the edge — the trait three specs already assumed
(persist offload, the lake roads, Arrow handoff). Keys are opaque
strings with `/` as the conventional separator: the S3 model, which
is the model.

| | |
|---|---|
| `Blob` | `put` (streamed in, `Etag` out) / `get` (chunks are the body, the ANSWER is the outcome — an absent key is a Left naming itself) / `head` / `list` (paged, key order) / `delete` (idempotent) |
| `Fs` | stage 0, jvm: a rooted directory — keys resolve STRICTLY under the root (`..` refuses), puts land in `.tmp` and MOVE atomically, crash leftovers invisible |
| `S3` | stage 1: PUT/GET/HEAD/DELETE/ListObjectsV2 path-style over the one http client — MinIO, R2 and AWS all fit; gets stream; a put holds ONE PART (`partSize`, 8 MiB): a longer body goes up as a multipart upload, aborted if the put fails, so no half object is ever visible (s3-multipart-put). `pending(key)`/`abandon(key, id)` clean up after a process that died mid-put — by exact key, which is all MinIO lists |
| `SigV4` | OWN signing — an HMAC chain over a canonical request, pinned by the AWS documentation's vectors, because four commands' worth of REST does not justify an SDK |
| the Source road | `putSource` / `getSource` — the same engine reached through `Source[Chunk[Byte]]`, where the answer is `Unit` and the element type is in the SIGNATURE; `getSource` still answers the outcome. Concrete on the trait: no engine changes |
| the plain road | `putBytes` / `putChunk` / `getBytes` — what most callers hold; storing a file no longer requires the producer algebra |
| `Bytes` (jvm) | `file` / `stream` / `fileSource`, 64 KB a time, opened when the program RUNS; `putFile` on any Blob. The read loop `Backup` had privately, once a consumer had copied it verbatim |
| `Backup` | incremental closed-segment copies of a persist store to any Blob engine; restore = place files back for ordinary recovery; okay-persist's `Doctor` certifies the copy offline. Its stream is `Bytes.file`, its walk `Producer.each` |

The same `BlobContract` suite passes over fs and live MinIO — both
roads, since 2026-09-16. A recording transport proves the secret key
reaches the HMAC chain and nothing else.

**Why two roads.** `put` is typed on `Produce`, the identity
signature, so the element type sits in the ANSWER position and
`pure(chunk)` type-checks — and emits nothing, a producer's `Pure`
being its end. A consumer stored a zero-byte object under the right
key that way, with two symptoms from the one cause: a size test that
never matched, and a restore that answered `refused: no header`. On
the Source road the same line compiles too — value discarding turns
the chunk into `()` — but as a DISCARD the compiler can flag, where
the Produce form is an ordinary answer nothing can. The element type
in the signature buys a warning, not an error; specs/blob.md "The
Source road" has the measurement and the whole argument.

`S3.wired(endpoint, bucket, region, creds)` is the engine awaiting
the one http client: `Http ?=> S3`, for `provide(http) { ... }`
edges and catalogs of preconfigured stores.
