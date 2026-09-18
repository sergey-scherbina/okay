package okay

/** one thread: a transaction is atomic by construction, so the
 * direct handler runs it against the cells with no log (specs/stm.md) */
given Stm[Async] = Stm.direct
