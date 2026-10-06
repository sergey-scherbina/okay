# Create-only object storage

## Overview
Bounded immutable chunks and commit markers need server-side exclusive creation,
not HEAD followed by unconditional PUT. Existing Blob operations are unchanged.

## Interface
ConditionalObjects exposes create(key, bytes): Created | Exists and
read(key, maxBytes): Option[Array[Byte]], both in Async. S3 implements it.
Objects are bounded to 8 MiB per create; callers chunk larger payloads.

## Behavior
- [ ] Create signs and sends If-None-Match:*; successful PUT returns Created.
- [ ] Only HTTP 412 returns Exists; 409 and all other failures throw, never overwrite.
- [ ] GET returns None only on 404; other errors throw.
- [ ] GET enforces the byte limit while reading and releases responses on failure.
- [ ] Invalid limits and oversized writes fail before network IO.

## Decisions
- Additive capability separate from Blob: no fake check-then-write default.
- Bounded chunks avoid changing the existing multipart protocol; no multipart
  conditional completion or automatic retry is claimed.
- Lost responses remain ambiguous; the caller must read and compare stored bytes.

## Out of scope
WORM, bucket configuration, delete protection, financial publication semantics.

## Results
Pending scoped verification.
