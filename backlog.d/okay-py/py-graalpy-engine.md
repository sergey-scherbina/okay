- [ ] py-graalpy-engine — Python IN the JVM, behind the SAME `PyEval`
      seam, as specs/py.md already designed ("one more engine behind the
      unchanged seam"). What changed since that spec: GraalPy is now
      Python 3.12/3.13 compliant, on Maven Central
      (`org.graalvm.python:python-embedding`), runs on a stock JDK, and
      has EXPERIMENTAL native-extension support (numpy below 2.3). So:
      pure-Python code in-process (no process start, no JSON hop), the
      subprocess kept for the C-heavy stack. In-process also opens what
      a subprocess cannot: a Python GENERATOR yielding operations walked
      by `okay.Foreign` — Python performing okay effects, one-shot.
      Measure a call against PySubprocess/PyWorkers before claiming it
      is faster: Truffle warm-up is real.
      PROBED 2026-09-25 (outside sbt, java + coursier jars): our
      UNCHANGED shim.py runs inside GraalPy 25.4.4.1.1 (Python 3.13.14)
      with the Context's stdin/stdout on Java pipes — so the in-process
      engine is `WireLink` streams over a Context, and every ForeignWorker
      feature comes for free. But on a stock JDK it is SLOWER on both
      axes: start 3.1 s against CPython's 57 ms, a call ~51 us after
      6000 calls against a pipe to CPython's ~11 us. The JIT is not
      reachable here: 25.4.4.1.1 needs a newer JVMCI than Temurin
      25.0.4.1; 25.0.4.1 with `-XX:+EnableJVMCI --upgrade-module-path`
      dies at Context creation ("VM config values missing ...
      NMethodPatchingType::conc_data_patch"); 24.2.2 on the GraalVM
      21.0.11 JDK finds no stdlib (`No module named 'json'`). Worth doing
      only on a GraalVM JDK of the matching release, and only if a
      measurement there beats 11 us/call; the one thing it offers that a
      subprocess cannot is a Python generator walked in-process.
