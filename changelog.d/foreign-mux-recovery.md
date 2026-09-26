## foreign-mux-recovery — recovery with several programs open on one worker (2026-09-26)

Part 4 of foreign-mux-duplex, closing it. With the wire multiplexed, two
programs can be open on one worker from two threads; killed, both came
back through the supervisor — and one of them failed ("continuation … is
not held here"), because both reopened the worker and one filed its new
continuations under the other's. The supervisor now reopens under its
lock and records each continuation against the generation of the worker
it actually ran on, the far-side call outside the lock. CrashConformance's
new case, red first on Go and Rust, is green three runs out of three.
`Durable` needed nothing: each run journals its own program. The
host-driven stream and the duplex transform are filed as
foreign-host-streams. specs/foreign-one.md Decision 25.
