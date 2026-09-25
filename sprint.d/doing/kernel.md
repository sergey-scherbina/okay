- [ ] kernel — a microkernel mechanism (operator, 2026-09-25: «сделать в
      окей общие механизмы для микроядерной архитектуры и потом ее
      использовать у нас … Сервис лоадер и контракты версий тоже»).
      A new okay-kernel (depends on the core only): Version and Range
      (the version contract), Port[A] (a typed, versioned contract,
      One or Many, with laws), Plugin (id, version, the kernel range it
      was built for, needs, provisions), Kernel.plan (pure: every
      problem as a value — missing, incompatible, ambiguous, cycle,
      duplicate, kernel mismatch — or a start order) and Kernel.start
      (a Resource: acquired in order, released in reverse, laws checked);
      on the JVM Discover.services (ServiceLoader, a broken provider is
      a problem not a crash) and Discover.jars (a plugins directory).
      And OkayModules in okay-deploy's sbt plugin: forbidden module
      edges refused at load (okay's own rules, then okay-watch's).
      Spec first: specs/kernel.md.
