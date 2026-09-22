## form-drill - a form one level at a time, and a form over a typed cursor

The operator asked how zippers meet form navigation and said a product
with a deep record is coming ("будет", 2026-09-23). The code answered
the first half before anything was built: a form's router, its errors
and its widget keys are all DOTTED PATHS, so a drill-down form's
cursor is that path, not a zipper — specs/form-drill.md, Decisions.

- `Form.renderAt[A](value, path)`: the value at a path, ONE level —
  the focus's scalars as widgets, its composite fields, list items and
  sums as `<key>$into` buttons, every key prefixed by the path so an
  event from the sub-form folds through `edit`/`submitted` unchanged.
  A second MODE of the existing render algebra (`RenderAlgebra(drill)`,
  three arms differ, everything else shared by construction); a root
  list renders its items, which the flat form calls unsupported.
- `Form.drill[A](value)(done)`: the screen — `into` pushes the path,
  `out` pops, `done` answers when `errors` is empty and otherwise
  shows them (the focus's own above the sub-form, those below a
  way-in under its button); `drillValue[A](a)(done)` over an existing
  value through the codec.
- `Form.askFrom(message, initial)`: `ask` seeded from a value — the
  seam `TestFormOptic` encoded by hand; `Form.askAt(cursor, message)`:
  the form of a `TypedZipper`'s focus, answering the cursor with the
  focus replaced — the typed zipper's consumer in forms, where the
  CODE chooses the part (a drill-down UI chooses at run time, which a
  compile-time position cannot follow: Decisions).
- `TestFormDrill` (11): the three focus shapes, the off-schema path,
  THE LAW (drill scripts with moves == the flat fold, four scripts
  over product/list/sum), the screen's moves and errors, `drillValue`,
  `askFrom`, `askAt` through a scripted `Dialog` host.
- docs: frontend-guide §3 (the deep-record paragraph), the okay-ui
  module page, theory ch. 10's zipper section (where a form's cursor
  is the path and why).

Gate `affected master` green, no warnings.
