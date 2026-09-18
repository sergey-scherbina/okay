- [ ] di-needs-from-static — `okay-di` asks a module's author to
      DECLARE what it needs; `Static.leaves` answers what a program may
      perform before it runs (specs/applicative-static.md), so the
      needs can be derived from the program instead. P13 item 4. The
      shape to settle first: a module's body is not a `Static` today,
      so either the declaration stays and `leaves` CHECKS it (cheap,
      catches drift) or the body is written as a spine (honest, and a
      bigger change). Trigger: the next time a declared need and the
      code disagree.
