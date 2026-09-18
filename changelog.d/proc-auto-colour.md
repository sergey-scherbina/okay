## proc-auto-colour - a question reads as its answer

`direct` has had auto-colouring behind a capability since
specs/direct-auto-coloring.md; the arrow road has it now too, so a
`Proc.direct` block needs no `!`:

    import okay.Proc.given
    val booking: Wf.Proc[String, String, Unit, String] =
      Proc.direct: _ =>
        val city: String = ask("city?")
        val t: Long      = now
        s"$city/$t"

THE GATE IS THE CAPABILITY. The entry's block is now
`ProcCtx[F] ?=> X => Y`, the conversion requires `ProcCtx[F]`, and its
constructor is `private[okay]` - so outside a block a question is not
a value, and the refusal is at compile time rather than a phantom
throwing at run time.

ONE conversion where `Direct` has two: a term's leaves are operations
of ONE signature, so the self/operation split has nothing to say here
and the `Effect` marker has nothing to add - the capability already
names `F`.

IT IS AN IMPORT, not a default, and that fell out of where the given
can live: `Free.directColor` needs none because `Free`'s companion is
in the implicit scope of `Free[R, A]`, while a block's source type is
the AUTHOR's signature, whose companion this library does not own. The
consequence is worth having - a file that did not ask for colouring
cannot get it by accident, which TestProcForms pins by NOT importing
and asserting the error is still there.

THE ONE FAILURE COLOURING CAN PRODUCE SILENTLY IS NOW A COMPILE ERROR.
The conversion fires where an ANSWER is expected, and `"a" + q`
expects nothing in particular - `String.+` takes `Any` - so the
question is stringified and the program asks one where it reads as
asking two. Measured before the check existed: `ask("left?") + "|" +
ask("right?")` answered `l|Ask(right?)` and asked once. Nothing of the
signature's type may now survive a rewrite, and what does is refused
with both fixes named.

The check cost three wrong cuts, and the way it was wrong is the
keeper: "skip the root" skipped an `Inlined` WRAPPER and then reported
the question under it, refusing every marked val in the repository.
The version that stuck counts questions, with wrappers transparent.
