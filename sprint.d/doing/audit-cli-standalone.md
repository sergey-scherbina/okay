- [ ] audit-cli-standalone — stage 2 of okay-audit: a standalone JSON CLI
      for Maven/Gradle, without SBT. It accepts a manifest describing module
      names, layers, class directories/jars and package-prefix layers, plus
      named allow exceptions; writes the existing text and JSON reports;
      refuses malformed input by name. Keep the scanner and report model in
      `okay-audit`; only the input boundary is new. Done-when: an executable
      CLI contract is tested against the fixture classes and a documented
      Maven/Gradle invocation needs neither SBT classes nor SBT settings.
