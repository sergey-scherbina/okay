## r-arrow-verify - shim.R's Arrow calls against a live R + arrow: 6/6, unchanged

r-arrow landed with its R side UNVERIFIED (no R with the `arrow` package
on the box). The same day, in a container (`r-base:4.4.1` +
`r-cran-arrow`, which pulls R 4.6.1 and arrow 25.0.0):

- `TestRArrow` 6/6 and `TestRMapReduce` (the map in R over the cluster's
  in-process workers) pass, with NO change to the three calls the caveat
  named — `t$schema$metadata`, `tab$metadata <-`, the
  `BufferOutputStream` round trip are the package's ordinary API.
- `RArrow.rscript` builds that image itself where docker is present
  (`TestR.container(image, packages)`, the jsonlite road parameterised),
  so the suite runs wherever `TestR` does, no env var needed.
- FOUND, not about Arrow: a raw column of a frame came back from R as
  base64 TEXT on the JSON road — `enc_col` wrote a raw(1) cell without
  its `{"t":"raw"}` tag. The columnar-wire box "a raw column round-trips"
  had been true Scala-to-R only. Fixed in the shim; the test reads the
  bytes back.
- FOUND: a container wrapper must BAKE the tmpdir in — `RSubprocess`
  starts the shim with a cleared environment, so `$TMPDIR` is empty
  there and the wrapper mounts `/tmp`; the shim file is then unreachable
  and the host reads "the shim says v-1". `TestR.containerShim` always
  did this; written down so the next wrapper does too.
