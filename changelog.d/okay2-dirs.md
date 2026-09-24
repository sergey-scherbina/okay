## okay2-dirs - the interop directories beside okay2/src

`okay2/okay2-cats`, `okay2/okay2-fs2`, `okay2/okay2-zio` are
`okay2/okay-cats`, `okay2/okay-fs2`, `okay2/okay-zio` (operator's
layout): inside `okay2/` the `2` in a directory name said nothing. The
artifact names are unchanged (`okay2-cats`, `okay2-fs2`, `okay2-zio`),
as are the packages (`okay2.cats`, `okay2.fs2`, `okay2.zio`). Only
`okay2/build.sbt`'s `file(...)` paths moved.
