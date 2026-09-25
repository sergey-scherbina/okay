## stack-safety-ui - a browser's path, a session's journal and a document under edit no longer walk the stack

Stack-safety stage 6 (okay-ui, okay-ui-gtk, okay-js), 49 rows. Three
walks were fed from outside the program and each overflowed at the
depth a long-lived client or session reaches (`TestUiDepth`, red first):

- `Form.focusAt`, the read-only twin of `editAt`, descended once per
  segment of a browser's dotted path inside a `flatMap` — `editAt` had
  been trampolined for exactly that reason (form-recursive-depth-safety)
  and its twin had not. A tail loop now; `renderAt` follows a
  100 000-segment path.
- `Sessions.segments` recursed once per connection that closed in a
  session's journal, which grows for the session's whole life. A loop
  now; a journal that closed 200 000 times refolds.
- `JsonEditor.outline` recursed once per level of the document under
  edit. Preorder on an explicit stack now, 100 000 levels, with the path
  held reversed (one cons per level instead of a copy of the path so
  far) and the indentation capped at 64 levels.

The other 46 rows carry their bounds: the Ui/Frame/React/Telegram/
Swing/Gtk walks descend the program's own view, `editAt` its
threshold-then-Cont road, `Protocol.describe` its reserve-before-recurse
guard, and okay-js's `Direct` is a macro over the user's source.
