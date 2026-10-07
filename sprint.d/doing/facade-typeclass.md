- [ ] facade-typeclass — THE FACADE DEFINED ONCE, over the typeclass
      (the operator, 2026-10-07: "!, +, %, Pure, pure и т.д. определить
      один раз в одном месте так чтобы это были алиасы над тайпклассом
      который зависит от контекста и указывал на реальные типы из
      конкретного пакета и мог переопределяться — фасад со статической
      диспетчеризацией"). `trait Facade[M[_ <: Row, _]](using Effects[M])`
      in the core: `type ![A, R] = M[R, A]`, `pure`, `effect`, `perform`,
      `handle`, `value` through the instance; `object machine extends
      Facade[cont.Free]` is the default (`export machine.*` at the core's
      door), `okay.freer.classic extends Facade[Rowed]` the tree's, a
      third is one line; the choice is the import, at compile time. Then
      the loose ends of stages 45–47: `okay.cont.Bang` folded into the
      core's facade, `Removed.rest` out of the machine's evidence, the
      Native heap and the suggestion-crash hint in the gate, the import
      loop into scripts/, okay.freer.Effects.scala split, docs prose.
