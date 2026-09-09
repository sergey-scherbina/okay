# okay-ui-gtk

> GTK 4 on Scala Native over the same `Backend` seam the DOM and Swing
> hosts implement (specs/frontend.md, ui-gtk): the tree is the plan,
> `Ui.patch` keeps a mirror, a patch is dispatched by what the mirror
> says is at its path, so no widget is ever type-tested. Present only
> where `pkg-config --exists gtk4` answers.

Depends on: `okay-ui` (Native). Scala Native only. The root build
AGGREGATES this project only when pkg-config finds GTK 4 at load
(`brew install gtk4 pkg-config` on a Mac, `libgtk-4-dev` on Debian);
on a box without it `sbt test` never sees the module, and the linking
flags are pkg-config's own.

## Guide

| | |
|---|---|
| `Gtk4` | the handful of `@extern` calls a level-L renderer needs — windows, boxes, labels, entries, buttons, scrolled windows, signals, `g_idle_add` |
| `Gtk.backend(container)` | the patch Backend: paths walk first-child / next-sibling; signal handlers are static C function pointers over a global widget → key table, so there is one live GTK backend per process |
| `Gtk.host` | `Ui.diffing` over the backend — semantic nodes arrive lowered, Form is level L |
| `Gtk.window` | the one thing that needs a display: pumps GTK's loop on its own thread until the application ends; a patch from another thread is marshalled through `g_idle_add` |

An application written once against `okay.ui` runs unchanged on the
terminal, under React, on the raw DOM, in a Swing window, in a GTK
window, and over the wire to a browser or the Compose client.

## Gotchas

- `gtk_scrolled_window_get_child` returns the GtkViewport GTK wraps a
  non-scrollable child in; `Gtk.scrolled` unwraps on both sides, or a
  SetText hits the viewport (found by a Gtk-CRITICAL, not by the
  law).
- `gtk_widget_activate` does not emit `clicked` on an unrealized
  button; the tests emit the signal by name and close the channel to
  read every event — two suite runs hung on that before.
- Weights are hexpand/vexpand; a multiline input is a plain entry; an
  image is a label. Recorded, not hidden.

## Tests

`TestGtk` (3) runs against real GTK widgets and skips with a message
when `gtk_init_check` fails: the DOM law battery, a keyed shuffle
moving the same pointers, signals coming back by key while a patch's
own change is not a user.
