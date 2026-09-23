- [ ] py-jython-engine — a SEPARATE module, okay-jython, for whoever
      needs it (operator, 2026-09-23: "Jython это если кому нужно,
      отдельным модулем. Пусть будет."). Checked the same day: Jython's
      release is still 2.7.x, PYTHON 2 ONLY; its Python 3 work is
      unreleased; no C extensions, so no numpy — specs/py.md's verdict
      for DATA work stands, and okay-py/py-graalpy-engine stay the
      Python 3 roads. The audience is existing Python 2 scripting inside
      a JVM application. In-process, so the deep bridge is available
      here too: a `Foreign.View` over Jython generators (Python code
      performing okay effects, one-shot), plus a `PyEval`-shaped call
      handler so a caller can swap it for okay-py's. Its own module
      because its standalone jar is tens of megabytes and nobody else
      should pull it.
