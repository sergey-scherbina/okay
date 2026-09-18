- [x] openapi-queries — DONE 2026-09-11 as openapi-parameters, and it
      was two gaps rather than one: the query declarations AND the
      path parameters' kinds, both for the same reason. `Router.Entry`
      took `route.describe` — a STRING — so the renderer re-parsed
      `{name}` out of the template and called every path parameter a
      string; `Route[Int]("id")` was published as text. The entry now
      carries `Route.Described` (template + params + queries), and the
      value exists so the two can never be passed apart again. A
      `Param` carries its own JSON Schema, through okay-codec's
      `JsonSchema.of` rather than a second mapping, and a custom
      `Param` may override it (`format: uuid`) — tested.
