- [ ] bang-is-cont — after classic-package, option 1: in the core
      `infix type ![A, R <: Row] = okay.cont.Free[R, A]` with `%`, the doors
      `pure`/`effect`/`perform`, `handle` over it, `Effects` for it
      (`Effects[Prog]` exists at the machine's carrier; the row form is
      `Free[R, A]` with `Has`), `reify`/`reflect` as the bridge to the
      classic through `Union[R, X]` (stage 40: a nominal row as the classic
      union). Keep: handlers apply in any order (`Removed`, stage 39), the
      same effects in another order are one program (`Free.reordered`),
      `widen` written for fewer effects. The machine's library
      (okay-cont: state, reader, writer, throws, choose, collect/generate,
      dialogue — stage 35) is the library of the new `!`.
