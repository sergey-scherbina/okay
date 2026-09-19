## federation-schema-door - a job refuses a coordinator whose schema it does not match, before it runs

Stage 3 of specs/federation.md: `Compat` at submission — a job whose
partial Schema a party cannot decode is refused before it runs. The
same shape `Cluster.guarded` gives to IDENTITY, applied to the wire's
SHAPE: `Cluster.schemaChecked(base)` wraps a `Serve` and refuses a
request whose coordinator-supplied schema digest disagrees with what
the party's own build would actually produce, before a byte of the
job runs.

THE PIECE THAT DID NOT EXIST: a `Schema` cannot travel between
processes. `SProduct`'s `make`/`parts` and every field's own thunk are
functions, and no codec serialises a function — so two processes that
do not share a build cannot exchange a live `Schema[A]`, only a
description of its shape. `okay.codec.Digest` (`federation-schema-door`'s
first commit) is that description, built by reading `Compat.walk`
closely enough to know it never touches a value — only names, nesting,
field presence, and whether a default exists.

THE DIRECTION THAT MATTERS. A party ENCODES its own partial with its
own live schema; the coordinator DECODES it with its own. The
question a party must answer before writing a byte is "will the
reader on the other end, with ITS schema, be able to decode what I
write with MINE?" — exactly `Compat.compare(mine, theirs).backward`,
reusing `Compat`'s existing, unchanged `Report`/`Verdict` machinery
rather than inventing a second one.

OPT IN, LIKE `guarded`, AND COMPOSABLE WITH IT. `Cluster.run` and
`Cluster.stream` compute the digest once per run (from `sink.wire`)
and attach it to every `Req.Extent`/`Req.Run`/`Req.Open` — the same
three request kinds `guarded` already gates by coordinator identity,
since these are the only ones that NAME a job (`Advance`/`Close` name
an already-admitted session). The digest ALWAYS rides along, cheaply
(one structural CBOR encode per run, not per partition or element),
but a party that never wraps its `Serve` with `schemaChecked` never
decodes it and pays nothing — mirroring exactly how `guarded` is
opt-in today. An EMPTY digest (every existing caller's default) skips
the check entirely, so a coordinator built before this box changes
nothing for anybody it talks to.

THE REFUSAL COMES BEFORE ANY READ — the same rule stages 1 and 2 both
already established for a foreign partition and an unrecognised
coordinator — and a job not found is left to the ordinary "no job
named" answer downstream rather than duplicated by this check.

8 tests, 3 with a negative control proving they fail without the
mechanism (a bypassed `schemaChecked` lets a mismatched schema
through), plus one proving composition with `guarded` still refuses a
stranger first, by identity, before the schema is ever inspected. 136
green in okay-cluster, JVM and JS, no compile warnings.
