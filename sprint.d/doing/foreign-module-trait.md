- [ ] foreign-module-trait — a whole Python module or R package behind
      a Scala TRAIT: `trait Stats { def median(xs: Vector[Double]): Double ! Py }`,
      `val stats = Py.module[Stats]("statistics")`, a macro generating
      one foreign-typed-calls call per method (the Retrofit/tapir-client
      move). And the reverse, as an sbt task: read a module's signatures
      and type hints (`inspect.signature`, R `formals`) and WRITE the
      trait, marking every untyped parameter so a reviewer sees what the
      hints did not say. After foreign-typed-calls.
