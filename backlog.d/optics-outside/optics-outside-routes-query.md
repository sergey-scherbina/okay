- [x] optics-outside-routes-query — DONE (2026-09-10, 6468e90a).
      Stage 3 of the spec: the query string, as its own `Query` type
      composed with `&` and handed over by `?`. The finding was a rule
      attached to the wrong layer — `Param.string` refused the empty
      string for the PATH's sake and broke `?tag=` the day the query
      arrived. `Route.Concat` also became `Route.Split` in the same
      lane, after the operator read the old name as the standard
      library's.
