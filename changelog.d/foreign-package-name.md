## foreign-package-name — okay.py is okay.foreign (2026-09-26)

The engine every wire language shares moved from package `okay.py` — a name
from when it served Python alone — to `okay.foreign`: the module's sources,
every dependent (okay-r, okay-rust, okay-foreign-cluster,
okay-foreign-workflow, okay-frege, okay-clojure), the living docs and the
refusal prefix. `okay.py` stays for a release as aliases of every public
name, so code still importing it compiles unchanged (TestPackageAlias).
The module keeps its directory and artifact, `okay-py`, for now:
specs/foreign-one.md Decision 26 says why.
