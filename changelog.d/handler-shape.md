## handler-shape - the cases' type picked per effect

The operator, 2026-10-02 ("Да"; specs/handler-forms.md).

- `Handler.Seen[F]` is derived from the effect before the cases are
  typed. It is `F[Any]` for an effect that has an operation answering its
  own field's type and none whose caller chooses the answer, `F[Answer]`
  otherwise. Such an effect (`Box`'s `Put(v: V) extends Box[V, V]`) is now
  written with `{ case … }`.
- Only an effect with both kinds stays beyond the case form, refused by
  name and pointed at `.poly`. Since state-get-update, State is not one of
  them.
