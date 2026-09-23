## optics-outside-live - subscribe to a lens

The last of the six triggers the operator lifted ("Все это нужно",
2026-09-23). The entry had said the lens must be reifiable and survive
serialisation; the arc had built that already without calling it so —
the dotted key of forms, policies, queries and a typed cursor's
`pathKey` is a lens in wire form, and `JsonOptic.path` compiles it
against a schema.

- okay-live `Watched[A](json)`: a document many viewers watch through
  paths. `subscribe(key)` answers a channel told the focused part
  each time THAT part changes; `set(key, value)` is the client's write
  through the same lens; `""` is the whole document; a key the schema
  does not write is refused by name; `put(a)` is the typed door
  through the codec; a focus that disappears is told `JNull` once.
- THE LAW (`TestWatched`, 4): over a history of edits, five
  subscribers at different depths each receive exactly the distinct
  consecutive values of their focus, computed from the history by the
  same lens — and nothing of anyone else's.
- okay-live gains okay-codec (a document is Json addressed by keys a
  Schema resolves). `ChatDemo` stays on its re-fetch, measured cheaper
  for its size; the consumer is the next product with a large document.
- specs/optics-outside.md stage 10, specs/live.md interface, the
  okay-live module page, frontend-guide §6, cursors-and-declarations §5.

Gate `affected master` green, no warnings.
