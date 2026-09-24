## okay2-eager - the tagless Effects[M] and the Eager encoding in okay2

okay's tagless interface is okay2's too: `trait Effects[M[_, _]]`
(`pure`, `perform`, `defer`, `tailcall`, `flatMap`, `map`, `foldCont`,
`runWith`) with the toolkit `object Effects` as its companion and `Free`
as its first instance. `Eager` is the second, as in the core: a pure
computation is its own value, so a bind chain evaluates at construction;
opaque outside `EagerModule`, opted into with `import Eager._`
(specs/okay2.md stage 19). 6 tests.

Docs: docs/okay2.md section 23.
