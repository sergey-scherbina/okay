- [ ] foreign-callbacks — Python or R calling BACK into okay in the
      middle of a call: `scipy.optimize.minimize` with an objective
      written in Scala, an R simulation asking okay's `Reader` for its
      parameters, a Python agent step writing okay's journal. That is
      the Perform direction of polyglot-remote-foreign; on the Python
      side it is one small module (`import okay; okay.perform(op)`) that
      writes a request on the wire and blocks for the answer. A callback
      is an okay OPERATION, so it is handled, mocked and journalled like
      any other.
