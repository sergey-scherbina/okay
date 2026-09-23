## ui-gtk-integration - okay-ui-gtk's Native suite runs under integrationTest, not the gate

Operator, 2026-09-23. On that day a gate hung twice at the Scala Native
runner's handshake with the linked `okay-ui-gtk-test` binaries, which
sat at 0.0% CPU with sbt blocked in a socket read. `TestGtk` drives a
real GTK widget tree, and that makes it Live by this repository's
definition.

A `Live` tag alone would not help: munit builds the suite to list its
tests, and the suite calls `Gtk.init()` as it is built, inside the
binary. So okay-ui-gtk's `Test / test` compiles the tests, which keeps
the warning check, and runs nothing. `integrationTest` runs `TestGtk`
(Live-tagged) by name. The module stays in the root aggregate, so a
compile break still fails the gate. specs/integration-test-gate.md.
