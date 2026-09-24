## okay2-zipper - Plate, Zipper and TypedZipper in okay2

The rest of okay-optics, in `okay2-optics`. First `Plate` (with
`Plate.of` a self-traversal) and Huet's `Zipper` over it. Then the typed
zipper: its frames are optics, `up` returns the parent's type, `field`
by name is a whitebox macro, and there are `at(i)`, `downCase`,
`asAffine`, `pathKey` and the type-changing `Poly` (specs/okay2.md stage
25). 24 tests.

Docs: docs/okay2.md section 29.
