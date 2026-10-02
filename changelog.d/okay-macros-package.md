## okay-macros-package - the core's macro implementations in okay.macros (stage 1)

The operator's ask ("перенесем макросы в пакет okay.macros", "move the
macros into the package okay.macros").

**Moved** to `src/main/scala/macros/`, package `okay.macros`:
- `ContMacro`, whole, by `git mv`;
- the implementations from `Shift` (`At.here`, `Shift.Key.of`,
  `Shift.Machine.of`), `Answers` (`derives Effect`, `Answers.flat`),
  `Distinct`, `Indexed` and `Provide` (`Module.plan` / `exports`), each
  into an object of its own.

**How:**
- The `inline def`s stay where they were and splice
  `okay.macros.X.impl`.
- Each object is `@publicInBinary private[okay]`, so it is closed to
  users by the compiler rather than by a comment. A private object
  reached from a public inline would otherwise get an unstable accessor
  (E192).
- No behaviour changes. The API files keep runtime code only, and the
  recursion inventory's 19 macro rows are re-filed under the new paths,
  with their bounds unchanged.

**Left:** Handler.scala's case-form macros, after handle-frames-forms
lands; and the satellites (okay-direct's `macros/` directory is still
package `okay`).
