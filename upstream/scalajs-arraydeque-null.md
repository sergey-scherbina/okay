# DRAFT, NOT SENT — Scala.js issue: `java.util.ArrayDeque` loses elements when it grows while wrapped

Status: drafted 2026-09-25. **Not sent** (operator: reproduce, do not
send yet). Target: https://github.com/scala-js/scala-js/issues. Checked
before drafting: `main` has the same `ArrayDeque.scala` as v1.22.0, and
no open or closed issue describes this (the nearest are #5257, "poll()
returns 0 when empty", and #4734/#4736, the ring-buffer rewrite).

---

**Title:** `j.u.ArrayDeque` drops elements when it grows while its ring buffer wraps

**Version:** Scala.js 1.22.0 (javalib `java/util/ArrayDeque.scala`, identical on `main`)

**Reproduction** (JVM and Scala Native print `33, 32, 31, …`):

```
val d = new java.util.ArrayDeque[Integer]()
for (i <- 1 to 33) d.push(i)
println(List.fill(33)(d.pop()))
// Scala.js: List(33, null, null, …16 nulls…, 16, 15, …, 1)
```

The smallest case: `addFirst(0)`, fifteen `addLast`, one more
`addLast`, then `pollFirst()` answers `null` where the JDK answers `0`.

**Cause.** In `ensureCapacityForAdd`, the branch for a FULL buffer that
wraps (`startIndex == endIndex`, not at 0) allocates a new array and
copies only `[0, endIndex)` into its upper half:

```diff
     } else if (startIndex == endIndex) {
       val oldCapacity = inner.length
       // move beginning of array to end
-      val newArr = new Array[AnyRef](oldCapacity * 2)
+      val newArr = Arrays.copyOf(inner, oldCapacity * 2)
       System.arraycopy(inner, 0, newArr, oldCapacity, endIndex)
+      Arrays.fill(newArr, 0, endIndex, null) // free the moved references for GC
       inner = newArr
       endIndex += oldCapacity
     }
```

The segment `[startIndex, oldCapacity)`, which is the front of the
queue, is never copied, so it reads back as `null`. A stack reaches this
branch on its 33rd `push`: the first growth, at 17 elements, is the
unwrapped `copyOf` branch, and after it `startIndex` wraps to the top of
the array.

**Verified.** A transcription of the ring buffer (push/pop, both
growth branches) against `java.util.ArrayDeque` on the JVM:
- as shipped: 16 nulls in 33 push/pop, and 192 wrong pops in a
  200 000-round mixed run;
- with the fix above: 0 and 0.

The 192 is exactly what the real Scala.js deque gave in the program
where we found it.

**Suggested test** (javalib `ArrayDequeTest`): push 33 and pop 33 and
compare with the JDK's order; also `addFirst` + 16 `addLast` + `pollFirst`.

---

Where this came from: okay's `Typed.fits`, rewritten as a worklist for
stack safety, failed on Scala.js alone with `MatchError: null`
(stack-safety-catch-up-okay2). `okay-sql`'s `ProbeScalaJsArrayDeque`
pins the behaviour on all three platforms.
