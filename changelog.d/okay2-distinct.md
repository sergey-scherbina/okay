## okay2-distinct - okay2 refuses a row with two signatures of one class

- A split tells signatures apart by their class, so `Ask[Int] +
  Ask[String]` is two types to the row and one to the split. Measured
  on the unguarded code first: `Handler.union[Ask[Int], Ask[String]]`
  compiled, and the String ask came back from the Int handler as a
  ClassCastException naming `Integer`.
- `Distinct[R]` (okay2/src/main/scala/okay2/Distinct.scala) is a
  blackbox macro over the row's intersection parents, as `Replayable`
  is: it drops `Row` and abstract parts and refuses two parts of one
  class that are not the same type. `Handler.union`, `Into.union` and
  `IntoZ.union` require it, as the Scala 3 core's `Handler.union`
  requires its `Distinct`; `Distinct.unchecked` is the escape hatch.
- NOT guarded, like the Scala 3 core: a per-signature handler over such
  a row (`State.handle` on `State[Int] + State[String]`).
- `TestDistinct` (3 tests): the defect kept under `Distinct.unchecked`,
  two refusals, the admitted shapes. 328 okay2 tests green cold; spec
  stage 10; docs/okay2.md section 3.
