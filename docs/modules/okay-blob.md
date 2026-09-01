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
| `S3` | stage 1: PUT/GET/HEAD/DELETE/ListObjectsV2 path-style over the one http client — MinIO, R2 and AWS all fit; gets stream, puts buffer while http's Body stays unstreamed (stated) |
| `SigV4` | OWN signing — an HMAC chain over a canonical request, pinned by the AWS documentation's vectors, because four commands' worth of REST does not justify an SDK |
| `Backup` | incremental closed-segment copies of a persist store to any Blob engine; restore = place files back for ordinary recovery; okay-persist's `Doctor` certifies the copy offline |

The same `BlobContract` suite passes over fs and live MinIO. A
recording transport proves the secret key reaches the HMAC chain
and nothing else.

`S3.wired(endpoint, bucket, region, creds)` is the engine awaiting
the one http client: `Http ?=> S3`, for `provide(http) { ... }`
edges and catalogs of preconfigured stores.
