- [ ] foreign-object-handles — keep a Python/R OBJECT on the far side
      and hold a handle to it: a fitted model, a tokenizer, an open
      dataset. Today only values cross, so a model fitted in one call is
      gone by the next. `PyRef` is a `Resource` (released → `del` on the
      far side); methods called by name, typed through
      foreign-typed-calls; in `PyWorkers` a handle PINS its worker, and
      a worker's death makes every handle it held throw by name (the
      dead-process rule, per handle). NOT journalable, said in the type:
      a handle names process state that a replay cannot rebuild, so a
      durable program keeps values, not handles. R: environments and
      external pointers the same way.
