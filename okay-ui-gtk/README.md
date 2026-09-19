# okay-ui-gtk

GTK 4 on Scala Native over the same `Backend` seam the DOM and Swing hosts implement (specs/frontend.md, ui-gtk): the tree is the plan, `Ui.patch` keeps a mirror, a patch is dispatched by what the mirror says is at its path, so no widget is ever type-tested. Present only where `pkg-config --exists gtk4` answers.

**Depends on:** `okay-ui` (Native). Scala Native only. The root build

This page is a pointer, not a guide: the module's own doc
below carries the pieces, the decisions and the measurements.

## Further

| | |
|---|---|
| [`docs/modules/okay-ui-gtk.md`](../docs/modules/okay-ui-gtk.md) | what it is, and the reasoning |
| [`specs/frontend.md`](../specs/frontend.md) | the design and its decisions |
