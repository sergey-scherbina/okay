- [x] optics-outside-ops-routes — DONE (2026-09-11, cbb1000f). Stage 4:
      the DESCRIBE interpreter's first consumer. A probe path lived in
      three independent literals (okay-ops served one, okay-deploy's
      `Health` defaulted to another, okay-script's ScriptDeploy wrote a
      third); the paths are values now and two tests hold the ends
      together. Found on the way: `Ops` compared the whole url while
      `Site` compared only the path, so `/healthz?probe=1` worked in
      one module and missed in the other. Also fixed `TestSignals`,
      which used a spin budget as a timeout and failed 2 of 3 runs on
      untouched master.
