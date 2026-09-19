## optics-outside — optics as an API, not an implementation (specs/optics-outside.md)

The arc the operator opened 2026-09-10, after `specs/optics.md` closed.
Optics INSIDE okay are done; this is optics, profunctors, arrows and
categories as the vocabulary a USER of the library writes. The spec's
criterion decides what belongs here and what is decoration: a
declaration earns an optic only when it must be given to more than one
interpreter and at least one of them DESCRIBES it instead of running
it — which a plain `S => A` cannot do. Read the spec's Overview before
taking any of these; it also records where the run-time tax lands (on
the USER's data path now, so a per-element optic ships behind `Fuse`
or not at all).
