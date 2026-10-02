## control-resume-node - control's resume, one closure less

Level 2 (operator's roadmap, 2026-10-02). The first `resume` of a
`Handler.control` clause defers to the Resume object itself, so no closure
of its own is needed. `maybe_control` is 37.96 µs and 438 KB against 38.96
µs and 462 KB (-24 B a resumed operation), 1.22x of `Maybe.option` now.
