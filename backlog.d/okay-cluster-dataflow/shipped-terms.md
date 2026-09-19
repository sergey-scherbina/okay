- [ ] **shipped-terms — a lambda on the cluster, without shipping a
      closure** (operator, 2026-09-18). `specs/dataflow.md` Claim 3
      says "nothing ships a closure" and states its price honestly:
      *you cannot type a lambda into a REPL and have it run on the
      cluster*; a plan whose leaves are anonymous functions runs
      locally and is refused at submission. The registry is the reason
      the whole class of `Task not serializable` failures does not
      exist here, and that is worth keeping.

      THE PROPOSAL IS TO LIFT THE PRICE WITHOUT LOSING THE CLAIM:
      ship the leaf as **TASTy** (Scala 3's typed AST) plus its
      captured environment as **CBOR** under a `Schema`. That is not
      Java serialization of a JVM lambda — no Kryo, no registration
      list, no synthetic `$anonfun$foo$1` to go stale — so the failure
      class Claim 3 eliminates stays eliminated. It is the Spark
      CAPABILITY without the Spark MECHANISM.

      WHY THE OBJECTIONS THAT KILL THIS ELSEWHERE DO NOT APPLY HERE.
      The book's Appendix A rejects TASTy+CBOR for durable workflows
      on four grounds, and a cluster inverts every one of them:
      a compiler at restore is AMORTISED (compile once per stage, run
      over millions of rows — seconds against minutes, where the
      workflow case spent seconds to save microseconds); symbols
      resolving against a classpath COSTS NOTHING because "every
      worker runs the same artifact" is already this engine's stated
      bargain; an executable payload is a different threat model on an
      authenticated internal control plane than in a persisted
      journal; and pinned code is a FEATURE — version skew inside one
      job is a bug.

      WHAT IS ACTUALLY HARD, and it is one thing: getting the term.
      TASTy is emitted for definitions at compile time, so a macro at
      the submission site must reify `'{ ... }` and marshal the free
      variables. That is bounded here in a way it is not for
      workflows — only the LEAVES need terms (`map`, `filter`, the
      fold), not a continuation across pauses — and `okay-staging`
      already runs `scala.quoted.staging` in production with the
      compiler isolated in its own module and an off switch.

      KNOWN CONSTRAINTS to design against: `staging.run` refuses a
      second thread, so a worker compiling several stages concurrently
      serialises on it; TASTy is forward- but not backward-compatible,
      which a single-artifact cluster satisfies anyway; and a leaf
      whose captured value is not `Schema`-able must still be refused
      at submission, with the stage named, exactly as today.

      ADDITIVE, NOT A REPLACEMENT. Jobs by name stay the production
      road — no compiler on the worker, fastest start, and the
      operational story the spec already tells. Shipped terms are the
      exploratory road: a REPL, a notebook, a one-off. Two roads, and
      the spec's Claim 3 gains a second sentence rather than losing
      its first.

      DECIDING TEST, before any of this is built: a lambda typed at a
      REPL, submitted, and run on a worker that never had that code —
      answering the same checksum as the registered job that does the
      same thing. If that cannot be made to work in a spike, the entry
      is closed with the reason.
