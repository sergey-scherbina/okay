## scalajs-arraydeque-null - Scala.js's ArrayDeque loses elements when it grows wrapped: reproduced, explained, report drafted (not sent)

- The cause is in Scala.js's javalib `ArrayDeque.ensureCapacityForAdd`
  (1.22.0, identical on `main`). For a full buffer that wraps, it
  allocates a new array and copies only `[0, endIndex)`, so the front
  segment `[startIndex, oldCapacity)` reads back as null.
- Smallest reproductions:
  - `addFirst` + 16 `addLast` + `pollFirst` gives `null`;
  - 33 pushes pop back as 33, then 16 nulls, then 16..1.
- `ProbeScalaJsArrayDeque` (okay-sql) pins the JDK's order on the JVM
  and Native and today's nulls on Scala.js. It goes red when Scala.js
  fixes it.
- The fix, `Arrays.copyOf` instead of `new Array`, was verified on a
  transcription of the ring buffer: 192 wrong pops as shipped (exactly
  the count of the original sighting in `Typed.fits`), 0 fixed.
- The issue draft is `upstream/scalajs-arraydeque-null.md`, NOT sent. No
  existing Scala.js issue covers it.
