## bind-name - `bindIn` is `bind`, `thenIn` is `andThen`

The operator's word, minutes after it landed ("А можно bindIn назвать
просто bind?", then "Да замени на andThen"). `p.bind(f: A => B ! G):
B ! (F + G)` and `p.andThen(q: => B ! G): B ! (F + G)` in Row.
`bind` was free (`Cont.bind(c)(f)` is a companion method, called
qualified, no extension in its way); `then` is a keyword in Scala 3,
so the answer-dropping form takes the name every Scala function
already has for "this, then that" — no `andThen` existed on programs
(the ones in Gen.scala and Validated.scala are their own types).
`TestBindIn` became `TestBind`; the guide and the spec section say
`bind`/`andThen`.
