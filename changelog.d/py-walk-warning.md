## py-walk-warning - the one warning stack-safety-py-r left on master

`Walk.up` (okay-py, Py.scala) discarded the frame `open.pop()` returns,
an E176 that a warm worktree hid from the lane's own gate and every cold
gate after it reported as RED ("no warnings, ever"). `val _ =` now.
