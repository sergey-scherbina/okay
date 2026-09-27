- drain-copy-per-element — DECLINED by design 2026-09-07 `Drain` is a
  case class and `Stream[Drain, Async].uncons` answers
  SUPERSEDED 2026-09-26 (merge-cap256-gap): `c.drained` is no longer
  `Writer.of(Drain(c))` — it is a hand loop over the received batch, so
  there is no `Drain` copy, `Some` or tuple per element at all on that
  road (12-14% fewer bytes on the merge). `Drain` and its `Stream`
  instance remain for callers that uncons a channel themselves.
