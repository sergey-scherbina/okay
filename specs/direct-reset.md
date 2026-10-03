# direct-reset — `Direct.reset` and `Direct.shift`: one door for both styles

Status: done, 2026-10-03. Operator: "а можно чтобы не нужно было писать
direct для прямого стиля в reset и в shift тоже. при этом чтобы монадический
стиль продолжал работать в них как и раньше"; the names qualified only,
`Direct.reset` / `Direct.shift` (the core's `reset`/`shift` unchanged).

## What

```scala
// direct style, no `direct`
Direct.reset[Int, S] {
  val x = Direct.shift[Int](k => k(1).? + k(10).?).?
  x * 2 + State.get[Int].?
}
// monadic style, the same door
Direct.reset[Int, S](for x <- shift[Int, Int, S](k => k(1)) yield x + 1)
```

## Design

- In `okay-direct`, beside `direct` (the core cannot reach the direct
  compiler). The operator first chose qualified names only; members of
  `Direct` cannot be kept out of `import okay.Direct.*`, which shadowed the
  core's `reset`/`shift` and broke TestShiftDirect — so (operator: "всё же
  Direct.reset/shift, перегрузив их") they take EVERY form the core's take:
  `reset[R, F]`, `shift[A]` (the block's evidence) and `shift[R, A, F]` (the
  key), and the files already importing `Direct.*` compile unchanged.
- The body is expected as `R | R ! G`. With `Any` a hand-written
  `direct { … }` inside lost its expected type and its monad (ambiguous
  givens); with the union it finds `R ! G`.
- The block's carrier is read off the block's own `DirectCtx[G]`, as the
  call site spelled it: rebuilt inside the macro it dealiased to `Freer[…]`,
  and the direct compiler found no Monad and could not widen a mark's row.
- `Direct.reset[R, F](body)`: the body sees the block's evidence
  (`Shift.Prompted`, so `shift[A]` and `Shift.exit` work) and the direct
  context (`.?`, `!`), and is typed WITHOUT an expected type. A macro runs the
  direct compiler over it: a body answering `R` is a direct block; a body
  answering a program `R ! Shift % R + F` comes out as a program of a program
  and is flattened — so a plain `for` passes through with one extra bind, and
  a monadic body may still use marks.
- `Direct.shift[A](k => body)`: inside a block (the one-argument form), the
  lambda's body the same way — a value of the answer type, or a program.
- Fully unmarked style (`k(1) + k(10)`) needs `scala.language.implicitConversions`,
  as auto-colouring always has; with `.?`/`!` no import.

## Behavior

- [x] `Direct.reset` with a direct-style body (marks), no `direct`
- [x] `Direct.reset` with a monadic body (`for`, `flatMap`), unchanged answers
- [x] `Direct.shift[A]` with a direct-style lambda body and with a monadic one,
      multi-shot included
- [x] the block's evidence inside: `Shift.exit` works in a
      `Direct.reset` body
- [x] fully unmarked with `implicitConversions`
