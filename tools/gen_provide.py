out = ['''package okay

/**
 * The installer half of the capability pair
 * (specs/context-functions.md, ctx-everywhere): expression-scoped
 * installation of givens — no given-line, no block nesting. With
 * the doors (APIs accepting `A ?=>`), this is the
 * dependency-injection story: compile-time resolution, given-scopes
 * as the object graph, zero framework. `provide(testEnv) { app }`
 * swaps a whole environment for a block.
 *
 * Nesting resolves to the NEAREST installation — inner provide
 * shadows outer, the E8-verified rule.
 *
 * Arities 1..22, GENERATED (the Cats mapN answer, applied: the
 * "unbounded" idiom is 22 fixed overloads, the platform's own cap —
 * ContextFunctionN ends at 22). The single-definition tuple route
 * is blocked by param-position match-type behavior, recorded as
 * E11/E12 in the spec; regenerate with tools/gen_provide.py.
 */''']
for n in range(1, 23):
    tps = ', '.join(f'A{i}' for i in range(1, n + 1))
    args = ', '.join(f'a{i}: A{i}' for i in range(1, n + 1))
    usings = ', '.join(f'a{i}' for i in range(1, n + 1))
    ctx = f'({tps}) ?=> B' if n > 1 else f'A1 ?=> B'
    out.append(f'''
inline def provide[{tps}, B]({args})(inline body: {ctx}): B =
  body(using {usings})''')
open('src/main/scala/Provide.scala', 'w').write('\n'.join(out) + '\n')
print('generated 22 arities')
