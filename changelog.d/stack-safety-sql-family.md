## stack-safety-sql-family - a pg array literal is bounded by Postgres's own MAXDIM; the SQL family's walks carry their bounds

Stack-safety stage 4a: okay-sql (`Typed`, `Row`), okay-pg, okay-r2dbc.

One real hole. okay-pg's `parseArray` recursed once per `{` of an array
literal read off the socket, so a literal nested 100 000 deep was a
StackOverflowError in the connection reader (`TestPgArrayDepth`, red
first on a 256 KB stack). The bound is Postgres's own: MAXDIM = 6
(src/include/utils/array.h) — the server refuses a deeper literal before
storing it and never sends one, so a literal past `PgSql.MaxDim` is
refused by name as damage from whatever is on the socket. Six dimensions
still parse, and the literal writer round-trips unchanged.

`Row.toParams` walked the HMap's tuple with a frame per column; it is a
loop now, and its row is paid. Every other row in the family is a
written bound: `Typed`'s `tpe`/`optional`/`decode`/`encode` walk the
finite Shape that `shapeOf` builds (a recursive product refused since
stack-safety-okay2-catch-up); pg's `colType`, `decodeCell` and `valueOf`
descend a type the catalogue keeps finite (a composite type cannot
contain itself, 42P16; arrays have at most 6 dimensions); pg's
`textOf`/`arrayLiteral`/`compositeLiteral` and r2dbc's `javaOf` walk a
value the program built; r2dbc's `valueOf` a driver array, bounded by
the DDL. Stage 4's remainder is okay-py and okay-r.
