## okay-cats-cross — okay-cats on JVM, Scala.js and Scala Native

Operator ask, 2026-10-02 (cats-depth audit). okay-cats is a crossProject
with one source set; the parking doors (`toIO`, `asIO`, `scheduler`) ask
`Answers[Async]`, so on JS they are a compile error at the call.
cats-effect 3.7.1 / cats 2.13.0 (the first with Native 0.5 artifacts).
JVM 323, JS 252, Native 252 tests green, the cats-effect laws included.
It found that the derived `tailRecM` on an eager carrier is NOT
stack-free (JS overflows at 300-1 000): sprint eager-carrier-depth.
specs/okay-cats-cross.md.
