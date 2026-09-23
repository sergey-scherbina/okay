## cbor-length-wraps: a CBOR length the bytes cannot hold is refused

`Cbor.In` used a string's declared length and a container's count as
the raw 64-bit argument: a byte/text string declared 2^32+5 long
narrowed through `n.toInt` and read five bytes; an array of 2^63+1
elements read negative and came back EMPTY; the skip path for unknown
fields had its own copy of both. Each desynchronised the rest of the
document inside a `Right`. Now one guard, `In.declared`, refuses a
length or count the bytes left cannot hold (one byte per item, two per
map pair) — for the typed reads, both skip paths and the staged codecs,
which share the reader. `TestCborLengths` run red first; no other CBOR
reader exists in the tree. Operator: "cbor-length-wraps сначала".
