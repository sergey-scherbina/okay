## ui-terminal-width - the row divides the SCREEN by weight, and a value wraps rather than runs off

`Resized` was an event no host ever sent, and `Frame.render` had no
notion of how wide the terminal is — so a weighted row divided its own
NATURAL width, which is the width of the text already in it. The spec
sentence "the terminal divides width by weight" was true of a width
nobody had measured.

`Frame.render(ui, focus, width)` takes a BUDGET now; `width = 0` is "no
budget" and is v1's layout exactly, so every caller that does not pass
one is unchanged — including okay-watch's `--tty` test. With a budget a
horizontal box divides IT by weight (separators off the top, the
remainder to the last child so the shares always add up), a vertical
box passes it through minus its padding, and a text WRAPS: at the last
space inside the budget for prose, at the budget itself for an IBAN or
a hash, which is the terminal's spelling of the browser stylesheet's
`overflow-wrap: anywhere`. A value that is cut cannot be checked
against anything, on either medium.

`Terminal.host` measures once with `stty size` and sends `Resized` as
the FIRST event, so an application can lay itself out differently on a
narrow terminal. Measured once, not per paint: a paint is not worth a
process spawn, and SIGWINCH needs a signal handler this file cannot
install — so a resize needs a new host today, stated rather than
pretended. A terminal that will not say its size answers None and the
host renders unbudgeted.

**THE COMPILER FOUND THE DEFECT THAT MATTERED.** Two recursive calls —
`Form` and the semantic catch-all — kept their default argument, so the
budget was dropped at exactly the nodes a page is made of: a `Table`
laid itself out naturally and ran past the screen. `-Wconf`'s E221
("recursive call used a default argument") named both, and a test pins
them now. The third warning was the same unused-default shape as the
`aligns` parameter before it.

TestTerminalKeys gains four cases.
