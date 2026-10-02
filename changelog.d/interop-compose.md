## interop-compose — one expression across cats, ZIO, kyo and okay

Operator ask, 2026-10-02, after interop-classes: functions written with
cats, ZIO, kyo and okay compose in one `>=>` chain and one `direct`
block, and okay functions are called from inside each library's code.
In: ONE `asOkay` (okay's, `okay.ToOkay` chosen by the value's whole type;
instances for `Future`, `IO`, `Task`, kyo `A < S`), on values and on
functions. Out: `asIO`, `asZIO`, `asKyo`, value and function forms.
`z.asOkay` moved from okay.zio to okay (same behaviour; import
`okay.asOkay`): per-module extensions of one name did not overload, and
kyo's implicit `lift` let its `asOkay` claim any value. TestMixed (6),
the three modules' suites green. Spec specs/interop-compose.md, guide
docs/effect-interop.md, "One expression across libraries".
