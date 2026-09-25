- [ ] ab-defaults-script-macos — `scripts/ab-defaults.sh` cannot run on
      this machine and could not be used by channel-default-adaptive
      (2026-09-25): it calls bare `sbt` (AGENTS.md: gate.sh is the only
      path), reads load from `/proc/loadavg` (Linux only; on macOS it
      silently reads 0 and never refuses), and uses `declare -A`, which
      the macOS bash 3.2 does not have. It also runs a whole lane REGEX
      per `Jmh/run`, which the per-lane-gated-jmh finding says never gets
      a quiet window here. What that lane used instead — one lane per
      `compare/Jmh/run` via gate.sh, arms alternating, the arm printed by
      the benchmark's `@Setup`, a lane accepted only at error <= 10% —
      is the shape to port into the script (or to replace it with).
