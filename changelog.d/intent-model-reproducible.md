## intent-model-reproducible - okay-intent's softmax uses StrictMath on the JVM, so the shipped model re-derives on every CPU

`TestModels` "the shipped artifact is exactly what the generator
produces" was red on x86 and green on the operator's ARM Mac, and was read
as an environment problem. It was a real one: the fit's softmax called
`Math.exp`, which may differ by an ulp between an intrinsic and fdlibm, and
the first double of the re-fitted model came out one bit apart.

- `Exp` per platform: `StrictMath.exp` on the JVM, `math.exp` on Scala.js
  (no StrictMath there, and nothing is re-derived there). `CharGrams` and
  `Probe` softmax use it.
- Re-running `MakeModel` through its refit gate wrote the SAME bytes as the
  committed `MeetingModel`: StrictMath agrees with what the Mac produced,
  so no model changed. TestModels is green on x86 now.
- okay-intent JVM 249 and JS 9 green; its dependents okay-dlm and okay-demo
  192 green.

Also in this lane, from building the release wave's scaladoc on all three
platforms (release-first-wave): `Diagnostics`' example escaped its `$r`,
the one warning left. The whole wave's `Compile/doc` is clean on JVM,
Scala.js and Scala Native.
