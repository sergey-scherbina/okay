## load-owned-thread-check — verify burner ownership rather than JVM-wide thread counts

Fixed in baf79a409: TestLoadStress captures and checks the actual Load-owned threads after normal and exceptional exit, retaining the exception and recording diagnostics. Three controlled unrelated threads reproduce the old assertion while every burner is already dead. The fixture releases and joins its unrelated threads. Scoped gate: okayTestJVM/testOnly okay.testkit.TestLoadStress, two tests passed without warnings. Load itself is unchanged.
