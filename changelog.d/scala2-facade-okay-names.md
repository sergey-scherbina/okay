## scala2-facade-okay-names - the facade speaks okay's names only

The operator asked the facade to drop its differences from the
originals. `okay-scala2` now spells everything as okay and okay2 do
(specs/scala2-facade.md stage 21):

- a new 2.13-compiled module, `okay-scala2-prelude`: a Scala 2 package
  object for `okay.scala2` carrying what the TASTy reader cannot see —
  the aliases `+`/`!`/`%`, `Pure`, `pure`, `choose`, `runChoice`, the
  runner `!.run`, and `runWith`. One `import okay.scala2._`, as on okay2;
  users no longer declare aliases.
- `State.run` runs to a value and `State.handle` handles; `State.set`
  and `modify` answer the new state (`put` gone); `Writer.run` answers
  `Seq`, `collect` the Vector; `Throws.runEither` (`run` gone);
  `Choose.choose`/`runChoice` and `Logic.observe`/`msplit`/`cut`/`ifte`/
  `gnot`/`interleave`/`fairBind` (`from`/`all`/`first` gone);
  `Async(a)` (`delay` gone), `Async.attempt` with okay's meaning and the
  old one as `Async.catching`; `Eff[-R, +A]`.
- 212 probe call sites and the Scala 2 docs migrated; the twin tests on
  the facade and okay2 are identical line for line, runner included.

Left different on purpose: the build (TASTy reader, two stdlibs) and a
user's own effect's declaration.
