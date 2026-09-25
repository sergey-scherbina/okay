## clojure-go-block-timeout-under-load - okay-clojure's go-block test runs in integrationTest

`TestCoreAsync`'s "a Clojure go block produces, okay consumes, in order,
to the end" crossed munit's 30 s in five unrelated gates on 2026-09-25
(cst-walk-stack-safe, http-mcp-agent-edge, kernel, okay-arrow,
mark-glyph-only), each at load 80-220 on 14 cores, and was green alone
every time (3.7 s at load ~100). Its 200 elements through a 4-slot
buffer are up to 200 park/unpark handoffs between core.async's go pool
and the test thread, each waiting for the OS scheduler, so its wall time
is a function of the box's load: a budget, not a defect.

A longer timeout or a smaller `n` would weaken the test, so by the
flaky-test policy (operator, 2026-09-25: move it to the integration
scope if it cannot be fixed under load) that ONE test is tagged `Live`.
The suite's other six, the ones the spec's mutants are written against,
stay in the default gate. Checked both ways: `okayClojure/test` runs 58
without it, and with `--include-tags=Live` it runs and passes.
