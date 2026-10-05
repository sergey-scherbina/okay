- [ ] jpms-deployment — JPMS stages B/C after jpms-boundary Stage A
      (specs/jpms-boundary.md). Stage B: okaywatch.* as a named module over
      one automatic okay assembly; jlink without java.sql/jdk.unsupported,
      illegal-native-access=deny, no add-opens; scanner and actual JVM
      restrictions must agree. Stage C in okay2: one package per module,
      descriptors everywhere, handlers as provides. Existing okay modules
      share package okay and cannot be separate JPMS modules. Stage A's
      report is evidence, not proof of named-module deployment.
