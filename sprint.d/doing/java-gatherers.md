- [~] **java-gatherers** — JDK 24 Stream Gatherers (JEP 485) in okay-java,
      specs/java-gatherers.md. Steps: (1) build — `.sdkmanrc` 21 -> 25,
      per-module `-java-output-version` 17/21 (compiler names the 21s),
      okay-java none; full matrix green on it. (2) `Gather.gatherer`/
      `Gather.stage`: Stage <-> Gatherer both ways, short-circuit and
      downstream rejection, law suite against okay's own run. (3)
      `Windowed.gatherer`, panes emitted as the watermark closes them.
      (4) docs with examples + literature. Operator ask, 2026-09-23.
