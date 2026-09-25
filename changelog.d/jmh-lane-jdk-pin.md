## jmh-lane-jdk-pin — JMH lanes run sbt on .sdkmanrc's JDK, as the gate does

`scripts/jmh-lane.sh` ran a bare `sbt`, which takes the PATH's java —
sdkman's global `current`, JDK 17 on this box — while `gate.sh` pinned
`.sdkmanrc`'s 25. Since the core gained a `versioned` Multi-Release
variant (`okayJdk22`, cont-stack-switch), dotc on 17 refuses it (`22 is
not a valid choice for -java-output-version`), and every JMH lane of the
core failed at once (found by cont-stack's A/B, 2026-09-25).

- `scripts/jdk-pin.sh`: gate.sh's pin moved into one sourced file;
  `JDK_PIN_ROOT` names the checkout holding `.sdkmanrc` (unset: the
  current directory, gate.sh's behaviour unchanged). `gate.sh` and
  `jmh-lane.sh` both source it.
- Checked with 17 first on the PATH: `sh` and `bash`, from another
  directory with `JDK_PIN_ROOT` and from the checkout, all resolve
  25.0.4.1. `jmh-lane-selftest` (whose fixture now carries the helper)
  and `gate-selftest` pass under `sh`; the former under `bash` too.
- Not changed: `scripts/ab-defaults.sh` still runs a bare `sbt`.
