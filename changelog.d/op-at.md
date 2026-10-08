## no conversion from an operation to a program

Lane op-at. The facade's `opToProgram` and okay-cont's `Op.toFree` are gone:
an implicit conversion warns at every use site unless the user imports
`scala.language.implicitConversions`. A bare operation where a program is
expected is written `op.at` (its row inferred from the expected type); an
operation bound by `flatMap`/`map` needs nothing, as before.
