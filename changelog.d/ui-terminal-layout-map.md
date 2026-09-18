## ui-terminal-layout-map - the renderer says WHERE it drew, and two limits disappear

`Frame.render` answered lines and nothing else, so hit-testing had to
SEARCH the frame for the text each focusable draws. That worked and
carried two limits, both stated where they lived: a widget whose text
wrapped was found by its first line only, and two widgets that render
identically were told apart by order alone.

`Frame.laid` answers the lines AND a `Placed` per focusable — key, row,
column, width, height — and `render` is its first half. ONE WALK, not
two: two walks of a layout convention are held equal by a law until the
day they are not, which is the lesson `ui-path-two-walks` measured its
way to for a different reason. `hit` and `focusLine` are lookups now,
and the marked-frame-minus-unmarked-frame trick that found the focused
line is gone with them — one mechanism instead of two.

A LEAF FILLS ITS CELL, not its text: a button in a column wider than
its label is still the button's column, so a click on the padding lands
where a reader sees the button.

**AND WRITING THE TEST FOUND TWO REAL DEFECTS, both in code that
shipped this afternoon.**

The first: only `Text` was ever wrapped, so an `Input` holding a long
value drew ONE line and RAN OFF THE SCREEN — in a renderer whose width
lane exists to stop exactly that. Every leaf wraps to its cell now.

The second, found by fixing the first: `wrap` counted CHARACTERS, and a
focused input carries its caret as reverse video — nine characters of
escape that occupy no column. A field that fitted in fourteen columns
wrapped into two lines of five and seven. `wrap` counts printable
columns now, which is what `Frame.width` has always meant by width.

okay-ui 201, okay-script 207. What is left of `ui-terminal-v2` is the
per-node viewport, which needs a decision about vertical space rather
than more mechanics — the entry says which one.
