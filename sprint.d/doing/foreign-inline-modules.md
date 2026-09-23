- [ ] foreign-inline-modules — Python or R source written NEXT TO the
      Scala that calls it: `val m = Py.inline("""def score(xs): ...""")`,
      turned by a macro into a resource module shipped in the jar and
      addressed by name like any other. The "no eval of a string"
      invariant of specs/py.md holds: the macro REFUSES anything but a
      literal (no interpolation, no runtime string), so the source is
      reviewed and versioned code and untrusted input still reaches
      Python only as data. `m.fn[In, Out]("score")` is then a
      foreign-typed-calls call.
