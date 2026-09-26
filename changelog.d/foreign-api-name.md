## foreign-api-name — Foreign is the host's API, Py its alias (2026-09-26)

The object holding the host's API for every wire language — `fn`, `hold`,
`program`, `callback`, `source`, `stream`, `releasing` — was `Py`, from when
it served Python alone; it is `Foreign` now, and `Py` is a `val` naming the
SAME object, so a type or a pattern written through one is the other's
(TestPackageAlias) and nothing written against `Py` changes. The main code
says `Foreign`; the facade generated for a Python module still writes `Py`,
Python's name for it, as Python's docs do.
