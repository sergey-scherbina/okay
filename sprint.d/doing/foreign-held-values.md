- [ ] foreign-held-values — `Objects[L]` for the Go, Rust and Haskell
      libraries: a small table of held values, so `call … held`
      answers a ref and `release` drops it (specs/foreign-one.md stage 6 as
      first written; narrowed by Decision 18). Python, R and TypeScript hold today;
      these libraries answer every call by value. Trigger: a caller whose
      far-side value is an object with state held across calls — a model,
      a connection, an open cursor for `PyStream.pulled` in one of these
      languages. Gate: WireConformance's held case (a counter held, called
      twice, released, then refused by name) over every row.
