## typed-path-key - the typed cursor's path as the form's key; the Take loop in a block, tested

Two hangovers of the arc, one lane (operator: "берись", 2026-09-23).

- `TypedZipper#pathKey: Option[String]` — `customer.address.city`,
  `lines[1].qty`: `Below` remembers the name `field` took, `Elem` its
  index, a case frame is transparent (as a case is to `Form.edit`),
  and a lens frame — which knows no name — makes the key `None` for
  the chain below it. `Form.drill(value, at)` opens at a key;
  `Form.drillAt(cursor)` opens the cursor's root at its focus and
  answers the decoded root — the position chosen by TYPE, the
  navigation done by the user.
- `backlog.d/okay-codec/schema-typed-paths` is ANSWERED and moved: the
  chain it asked for has been the typed zipper since stage 2; this
  adds the spelling it lacked. `optics-field-fuse` stays open.
- `for i <- Take.each[Int] do tell(i * 2).!?` in a direct block IS a
  `Stage` — asserted by running now, in okay-llm (`TestTakeLoopInBlock`,
  2), the module whose test classpath holds both okay-direct and
  okay-stream; the spec had said so by type only.
- Compiler facts recorded in specs/zipper.md stage 5: `.copy` on a
  frame re-infers its type arguments against an inline `name: L`; the
  `field` extension takes `z: Z` as `at` does.
- docs: guide §10, theory ch. 10, frontend-guide §3, one sentence each.

Gate `affected master` green, no warnings.
