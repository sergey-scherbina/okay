- [ ] aggregator-sum-hides-its-specialization — `sum[N]`'s declared
      return type is `Aggregator[N, N, N]`, so the `OfLong` underneath
      is invisible and `zipLong` cannot be reached from the idiomatic
      spelling (it needs `Aggregator.sumLong`). A match type on the
      return, or an `OfLong`-returning overload for the `Long` case,
      would close it. The same shape as `zip` hiding the
      specialization, one level down.
