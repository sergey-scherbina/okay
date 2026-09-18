## okay-blob — from a consumer (2026-09-16, okay-watch)

These four come from OUTSIDE: okay-watch backs its case log off the
volume through `Blob`, and hit one real defect doing it. Each entry
carries what produced it rather than a wish.

The defect, because all four point at it. `Blob.put` takes
`Chunk[Byte] ! (Produce + Async)`. `Produce` is the identity
signature, so the element type sits in the ANSWER position, and
`pure(chunk)` therefore type-checks — and emits nothing, because
`Stream[Producer, Pure]` reads `Free.Pure(_)` as the END of the stream
and discards its value. The result was a zero-byte object under the
right key. Two symptoms from one cause: the caller's size test never
matched so every backup pass re-copied, and the restore answered
`refused: no header`. Neither compiler nor runtime said a word; a
round-trip test found both. Nothing here is a bug report — the
algebra does exactly what it documents — but the wrong thing was the
one that type-checked, which is a shape worth removing.
