# Create-only object storage

## Overview
Bounded immutable chunks and commit markers need server-side exclusive creation,
not HEAD followed by unconditional PUT. Existing Blob operations are unchanged.

## Interface
ConditionalObjects exposes create(key, bytes): Created | Exists and
read(key, maxBytes): Option[Array[Byte]], both in Async. S3 implements it.
Objects are bounded to 8 MiB per create; callers chunk larger payloads.
Create also sends a signed Content-MD5 transport checksum, required by S3
for Object Lock retention uploads. SHA-256 remains the content identity.

## Behavior
- [x] Create signs and sends If-None-Match:*; successful PUT returns Created.
- [x] Only HTTP 412 returns Exists; 409 and all other failures throw, never overwrite.
- [x] GET returns None only on 404; other errors throw.
- [x] GET enforces the byte limit while reading and releases responses on failure.
- [x] Invalid limits and oversized writes fail before network IO.
- [x] Bounded PUT includes a correct signed Content-MD5 checksum.

## Decisions
- Additive capability separate from Blob: no fake check-then-write default.
- Bounded chunks avoid changing the existing multipart protocol; no multipart
  conditional completion or automatic retry is claimed.
- Lost responses remain ambiguous; the caller must read and compare stored bytes.

## Out of scope
WORM, bucket configuration, delete protection, financial publication semantics.

## Results
TestConditionalObjects: 2 tests passed. Affected Test/compile passed,
no compile warnings; recursion inventory holds. Integration with real MinIO
is verified by the financial cloud consumer, not claimed by this unit fixture.
