package okay

/** the parking platforms run transactions through TL2: versions,
 * CAS-owned commit, structural fast paths (specs/stm.md) */
given Stm[Async] = Stm.tl2
