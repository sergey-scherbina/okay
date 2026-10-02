## delimited-machine-suite — the machine's own suite, on every platform

Sprint cont-js-depth stage 2, 2026-10-02. The DPJS law suite moved to
scala-cross (JVM, Scala.js, Native); TestDelimitedDepth, through
`Delimited` alone: a million captures whose bodies use their
continuation's answer, a million nested delimiters, a million binds,
2^20 multi-shot runs — green on all three platforms and on a 128 KB JVM
thread. With `k` as data the machine needs no host stack per level; what
remains bounded is the strict-`k` bridge. specs/cont-js-depth.md.
