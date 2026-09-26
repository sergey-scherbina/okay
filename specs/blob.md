# The object-store seam (blob)

## Overview

Three landed specs already ASSUME an object store and none of them
names a seam for it — the audit's quiet gap: okay-persist stage 3
offloads cold segments "to object storage", the lake roads
(specs/data.md) read and stage files, r/py hand frames over as
Arrow files that a shared store would carry across machines. This
spec adds the missing small trait, in the persist mold: bytes and
streams in the engine, meaning at the edge, engines behind one
seam.

## Interface

```scala
package okay.blob

trait Blob:
  def put(key: String, bytes: Chunk[Byte] ! (Produce + Async)): Etag ! Async
  def get(key: String, range: Option[(Long, Long)] = None)
  : Either[String, Unit] ! (Produce + Async)   // chunks produced; the
    // ANSWER says how it ended — an absent key is Left naming the key
  def head(key: String): Option[Meta] ! Async         // size, etag, modified
  def list(prefix: String): Chunk[Meta] ! (Produce + Async)  // paged underneath
  def delete(key: String): Unit ! Async

  // the Source road and the plain one — CONCRETE, so no engine changes
  def putSource(key: String, bytes: Source[Chunk[Byte]]): Etag ! Async
  def getSource(key: String, range: Option[(Long, Long)] = None)
  : Either[String, Unit] ! (Writer % Chunk[Byte] + Async)   // told, outcome kept
  def putChunk(key: String, chunk: Chunk[Byte]): Etag ! Async
  def putBytes(key: String, bytes: Array[Byte]): Etag ! Async
  def getBytes(key: String, range: Option[(Long, Long)] = None)
  : Either[String, Array[Byte]] ! Async               // for an object that fits

// jvm
object Bytes:
  def file(path: Path, chunk: Int = 64 * 1024): Chunk[Byte] ! (Produce + Async)
  def stream(open: => InputStream, chunk: Int): Chunk[Byte] ! (Produce + Async)
  def fileSource(path: Path, chunk: Int): Source[Chunk[Byte]]
extension (b: Blob) def putFile(key: String, path: Path, chunk: Int): Etag ! Async
```

Streams both directions, chunked, constant memory — a 10GB segment
never lives in the heap. Keys are opaque strings with `/` as the
conventional prefix separator (the S3 model, which is the model).
Conditional put (if-absent / if-etag) joins when a consumer needs
optimistic commit over blobs — declared now so the signature grows,
not changes. Multipart upload is an ENGINE detail under the same
`put` (the engine splits at its threshold); it is not API.

## Engines

- **Filesystem** (stage 0): a rooted directory, keys as paths —
  tests, local runs, and honest single-machine deployments; the
  same trait so nothing above notices a promotion to S3 later.
- **S3-compatible** (stage 1): the REST subset this seam needs
  (PUT/GET/HEAD/DELETE/ListObjectsV2 + multipart) with **SigV4
  signing implemented over the existing HTTP client** — SigV4 is a
  documented, stable algorithm (an HMAC chain over a canonical
  request), well inside this stack's speak-the-wire habit, and
  doing it ourselves keeps the seam cross-platform and
  dependency-free. One implementation covers AWS S3, MinIO (the
  live-test double, TestLive pattern), Cloudflare R2, GCS's
  S3-compat endpoint — the S3 API is the lingua franca, which is
  why the OWN protocol is worth it here and a native GCS/Azure
  dialect is not (yet).
- TLS per specs/tls.md; credentials per specs/conf.md (access key
  id a field, secret key a `Secret` ref — never in a URL).

## Consumers (the reason it exists)

persist stage-3 segment offload (a closed segment is `put` once,
immutable — object stores' best case); the lake write road (staged
files for warehouse COPY, specs/data.md); r/py Arrow frame handoff
across machines; persist-backup copies (specs/persist.md, Backup).

## Behavior

- [x] put then get round-trips bytes at constant memory (a payload
      larger than any single buffer, asserted by allocation bound
      or by chunk accounting)
- [x] get with a range returns exactly the slice; head reports
      size and etag without a body
- [x] list(prefix) pages transparently and yields every key once,
      in key order
- [x] absent keys: get is an error value naming the key, head is
      None, delete is idempotent
- [x] the fs and S3 engines pass the same contract suite (StoreSuite
      pattern); the S3 side runs against MinIO and SKIPS where
      absent (TestLive pattern)
- [x] SigV4: a canonical-request test vector from the AWS
      documentation signs to the documented signature (the
      algorithm pinned by test, not by trust)
- [x] a secret key never appears in a URL, a log line, or an error
      (the conf invariants asserted at this seam)
- [x] a put holds ONE PART (s3-multipart-put, 2026-09-26): a body
      longer than `partSize` goes up as an S3 multipart upload, each
      part signed with its real hash; an object of four parts comes
      back byte for byte and its etag says `-4` (TestS3Multipart, Live)
- [x] a put that fails mid-way ABORTS its upload: no object, no upload
      left open; `pending(key)`/`abandon(key, id)` find and discard what
      a killed process left

## Out of scope

- native GCS/Azure dialects — their S3-compat endpoints first; a
  dialect when a deployment names one
- bucket administration (creation, lifecycle, versioning policy) —
  deployment's concern; the seam works within a bucket it is given
- presigned URLs — join with a consumer (the ui file-download
  story is the likely one)

## Decisions

- **A seam, because three specs already assumed it** — the audit
  rule: an assumption shared by three specs is an interface in
  denial. Rejected: each consumer hand-rolling its S3 calls.
- **Own SigV4 over an SDK** — the needed subset is small, the
  algorithm is stable and testable against published vectors, and
  the SDK's dependency tree buys nothing the seam exposes.
  Rejected: AWS SDK (JVM-only, heavy), jclouds (heavier).
- **S3 API as the one dialect** — it is the industry's shared
  surface; MinIO/R2/GCS-compat all speak it. Rejected:
  per-provider dialects now.
- **Multipart as engine detail** — callers say put; thresholds are
  tuning, not semantics. Rejected: a multipart API surface. KEPT when
  it was built (s3-multipart-put, 2026-09-26): `put` goes multipart by
  itself above `partSize`; the only additions are `pending`/`abandon`,
  the cleanup of an upload whose PROCESS died — a put that merely
  fails aborts its own. `pending` takes a key, not a prefix: MinIO
  lists an object's uploads only by its exact name, which a prefix
  test found by passing vacuously.
- **get answers Either, chunks ride the effects** — the absent key
  needed a place to BE a value: the produced chunks are the body,
  the program's answer is the outcome. (Adjusted at blob-fs; the
  original sketch returned the chunk type and had nowhere to say
  "no such key" without a throw.)
- **Immutable-object bias** — put-once keys (segments, staged
  files, frames) are the design center; overwrite works but
  nothing here builds mutable-blob coordination on top (that is
  what the LOG is for). Rejected: blob-level optimistic-locking
  machinery beyond the declared conditional put.

## The Source road (2026-09-16, blob-source-road)

A consumer found the shape of this seam's one trap, and the fix is
additive: nothing that implements or calls `Blob` changed.

**The trap.** `put` takes `Chunk[Byte] ! (Produce + Async)`. `Produce`
is the identity signature, so the element type sits in the ANSWER
position, and `pure(chunk)` type-checks — and emits nothing, since a
producer's `Pure` is its END and the stream instance reads it as
`None`. okay-watch stored a zero-byte object under the right key that
way; its size test then never matched, so every backup pass re-copied,
and the restore answered `refused: no header`. Two symptoms, one
cause, and neither compiler nor runtime said a word.

**The road.** A `Source[W]` is `Unit ! (Writer % W + Async)`: the
answer is `Unit` and the element type is in the SIGNATURE, so
`pure(x)` is a type error there rather than a silent nothing — a test
asserts both halves, the error on the Source side and the silence on
the Produce side. `Source.toProducer` and `Source.fromProducer` (core)
convert in one walk each, no Option or tuple per element; `putSource`
and `getSource` are those conversions on the trait. `getSource` still
ANSWERS the outcome, because that is where an absent key lives.

`fromProducer` names the element type separately from the answer type
because the identity signature cannot: `get` reads as a producer of
Eithers and produces chunks. That claim goes through `produced`, the
one cast the producer algebra rests on, and nowhere else.

**Reading a Source with a parameterised element type.** `Writer.run`
splits on `TypeableK[Writer % W]`, whose derived test at
`W = Chunk[Byte]` is an unchecked one (E092, the TypeableK caveat the
core documents at `Source.runCollect`). `Writer.collect` (core) is
`run` split the other way, on the concrete G, with the answer kept —
what a `getSource` is drained with, and the reason the contract
tests compile without a warning.

**The asymmetry, measured rather than hoped.** The first draft of the
core test asserted that `pure(1)` is a type error at `Source[Int]`.
It is not: value discarding turns the `1` into `()`, and it compiles.
What differs is what compiled — a DISCARDED value, which the
compiler flags under `-Wvalue-discard` (on in this build), where the
Produce form is an ordinary well-typed answer nothing can flag. The
element type in the signature buys a warning, not an error, and the
test says exactly that.

**The plain road.** Most callers hold an array or a file, not a
stream. `putBytes`, `putChunk`, `getBytes` and, on the jvm, `putFile`
and `Bytes.file` mean storing a file no longer requires learning the
algebra first — and `Bytes.file` is the 64 KB read loop `Backup` had
privately, which a consumer had copied verbatim. `Producer.each`
(core) is the walk that keeps the producer's answer, which `uncons`
loses at its `None`; three hand-rolled copies of it collapsed into
the one.

**What did not change, and why.** The `Produce`-typed `put`/`get`
stay the engine primitives. Re-typing them on `Source` would move six
types and every engine; the additive road buys the safety at the
call sites that had the trap, which is all of them outside the
engines, for no breakage. If the primitives are ever re-typed — for
`Flush.now` as a multipart boundary, say — the conversions here are
the two functions that would become the identity.

## Results (stage 0)

Shipped 2026-09-01 (blob-fs): trait Blob cross-built; the Fs engine
(jvm) — keys resolve STRICTLY under the root (`..` refuses), puts
land in `.tmp` and MOVE atomically so a reader never sees a
half-written object, crash leftovers are invisible, the etag is
engine-defined (size-mtime; content hashes arrive with S3's
protocol). One adjustment to the sketch, recorded in Decisions: get
answers Either — the produced chunks are the body, the program's
answer is the outcome, which is where an absent key can BE a value.
BlobContract is the suite blob-s3 re-runs. 7 tests.

## Results (stage 1)

Shipped 2026-09-01 (blob-s3): own SigV4 pinned by the AWS doc's GET
and PUT vectors verbatim; the list vector's remembered tail was
wrong and was settled by cross-implementation agreement (the Scala
signer and an independent Python one produce identical signatures on
the documented inputs — recorded here because the diagnostic is the
lesson: two vectors pin the algorithm, a third settled by agreement).
The engine speaks PUT/GET/HEAD/DELETE/ListObjectsV2 path-style over
the one http client; puts BUFFER (okay-http's Body is deliberately
unstreamed — when it learns streaming, multipart and constant-memory
puts arrive together; stated in the engine doc); gets stream. The
SAME BlobContract passes against live MinIO — round-trip, ranges,
list order, absent keys, overwrite — and a recording transport
proves the secret reaches the HMAC chain and nothing else.

## Results (s3-multipart-put, 2026-09-26)

A 15.1 MB object in four 5 MiB parts: put 120 ms, get 55 ms, against
MinIO in docker on this box — one part (5 MiB) in memory for the put.
Two findings on the way. MinIO writes the quote in a completed
upload's ETag as `&#34;`, which the XML unescape did not know, so the
etag came back wrapped in entities. And MinIO's ListMultipartUploads
matches exact keys only: the "no upload left open" assertion passed
with a prefix whether or not the abort ran; with the key, removing the
abort turns it red.
