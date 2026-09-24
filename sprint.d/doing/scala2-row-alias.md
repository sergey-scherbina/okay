- [ ] scala2-row-alias — `type +[R, G[_]] = R with Effect[G]` for Scala 2
      rows: a row on the left (kind *), an effect's operation language on
      the right, so `State[Int] + Console + Choose` reads like okay's
      `State % Int + Console + Choose`. Scala 2 cannot declare the
      core's `+[F[_], G[_]]` (an alias cannot answer a higher kind, and
      `Can[F with G]` is not `Can[F] with Can[G]` in 2.13, so handlers
      break — measured 2026-09-24 on a model). Lane: find a home 2.13
      can SEE (a Scala 3 top-level alias is invisible), use it in every
      Scala 2 row that names a user effect beside another capability
      (probe, docs), pin it in the probe. (2026-09-24)
