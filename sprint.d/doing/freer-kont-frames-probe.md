- [ ] freer-kont-frames-probe — PRIORITY: MEDIUM (operator, 2026-09-30:
      "промпт как хендлер" is the right design; this is its only
      stack-safe form). A PROBE, not a migration: `Freer` whose `Bind`
      holds a `Kont` instead of one function — a type-aligned queue of
      functions (van der Ploeg & Kiselyov, "Reflection without Remorse",
      Haskell 2014) CUT into segments by handler MARKS, each mark an
      installed handler as a VALUE with its state (`Mark(h, s)`), so ONE
      loop runs every handler:
        `Bind(m, k).flatMap(g) == Bind(m, k :+ g)`   (O(1), no rotation)
        `handle(h, s0)(body)   == Bind(body, Kont(∅, (Mark(h, s0), ∅) :: Nil))`
        `Bind(Return(x), k)`: pop a function; an empty segment is its
          mark: `h.ret(s, x)`, continue after it
        `Bind(Inject(op), k)`: walk SEGMENTS (not functions — a
          left-nested fold would make the walk quadratic) to the mark
          whose handler knows `op`; a tail-resumptive op (`h.tail`) swaps
          the mark's state and continues, capturing nothing; otherwise
          the segments before the mark ARE `k` (today's `split`); no mark
          = the op leaves into the residual program.
      Delim becomes an ordinary effect: `reset` a mark, `shift` an op
      whose clause gets the cut, `shift0` the cut with the mark, `dollar`
      the mark's `ret`, `Watched` a mark that counts its re-entries.
      WHY THE MACHINE CANNOT SIMPLY BECOME HANDLER LOOPS (found
      2026-09-30, before any code): a handler written as a loop over its
      body (`State.handle`'s shape) must CALL a nested handler's loop to
      see its output, so n dynamically nested handlers are n JVM frames
      — no `Delay` helps, the parent has to call the child to step it.
      `TestDollar` pins 100 000 nested `dollar`s in constant stack, and
      the operator's rule forbids unbounded recursion; the Delim machine's
      `Segs` is exactly the handler stack held as DATA in one loop. The
      same bound holds TODAY for every other handler (a `State.handle`
      nested in itself 100 000 times dynamically); nothing hit it only
      because ordinary handlers nest by the program's text. `Kont` makes
      that general: any handler, any depth, constant stack.
      ALSO REFUTED BY THIS, to correct when the lane lands: Delim.scala's
      header, "FIRST: push is an operation, not a handler application
      ... nested handlers cannot do it: an inner handler forwarding a
      shift it does not own would forward it OPAQUELY". Forwarding that
      WRAPS the continuation with the forwarding handler keeps its frames
      in the capture — `Delim.runNested` (delim-forward, 2026-09-17)
      does exactly that and TestDelim's nested-machine captures pass. The
      true reason for one machine is stack depth, not expressiveness.
      WHAT IS ALREADY REFUTED, and the bar it sets: the queue ALONE was
      prototyped and dropped (refuted-declined-or-answered/
      bind-continuation-queue, specs/map-fusion.md "The continuation
      queue"): folded back into one continuation at `resume` so handlers
      stayed unchanged, it moved no map-heavy lane and cost the map-free
      ones 1.01-1.03x (the node's class tests). This probe differs in
      what it removes, not in the queue: the handler loops, `relay`'s
      re-wrapped continuation per forwarded op per handler, `resume`'s
      rotation closure (the JIT-mode lead, freer-rotation-closure-jit-
      modes) and the Delim machine as a separate interpreter. It must
      win those back, measured, or it is dropped like the queue was.
      THE PROBE: a second `Freer` beside the real one (a test/jmh-only
      source set, nothing in main changes), with `State` and `Delim` as
      two `Handler` values. ORACLE: TestDollar's 100 000 nested dollars,
      TestDelim's family, TestLexical's depth tests, State's laws, all
      ported to it and giving today's answers; plus a State handler
      nested 100 000 deep dynamically (fails on today's tree, must pass).
      TYPE CHECK FIRST: the queue makes the bind's middle index
      existential between every pair of functions (today, once per
      Bind) — compile the indexed `Freer[G, S, R, A]` version before
      measuring anything. LANES (per-lane jmh, 6 forks, modes listed —
      freer-loop-jit-modes): FibBenchmark.fib100, HandlerBenchmark
      stateEffect / relayPrebuilt / handlePrebuilt / stateForward,
      DelimBenchmark's five lanes, BuildShapeBenchmark rowFoldM.
      DECISION after it: migrate the core (an arc of its own: every
      handler of the library rewritten from a loop to a `Handler` value,
      ~100 `Bind(Inject(e), k)` sites) or stay, and then decide
      delim-forwarding-default. Literature: Dybvig, Peyton Jones & Sabry,
      "A Monadic Framework for Delimited Continuations" (JFP 2007, the
      continuation as a sequence of frames and prompts); Xie & Leijen,
      "Generalized Evidence Passing for Effect Handlers" (ICFP 2021, the
      handler stack without search); Forster, Kammar, Lindley & Pretnar,
      "On the Expressive Power of User-Defined Effects" (ICFP 2017,
      handlers and delimited control express each other).
