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
