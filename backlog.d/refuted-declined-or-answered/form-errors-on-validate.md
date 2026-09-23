- form-errors-on-validate — ANSWERED 2026-09-18, and it was not the question it
  looked like (was filed as `item-058` in okay-codec; named and moved here by
  backlog-audit-0923). The entry read the difference
  between `Form.errors` and `Validate.errors` as WORDING, and proposed
  a `Wording` parameter on Validate. Running the two side by side on
  one schema (`FormErrorsProbe`) found two places where the form was
  simply WRONG and the accumulating decoder right, and both are fixed
  in the form: a field the SCHEMA DEFAULTS was reported "required", so
  the form held a submit that `Form.decode` accepts; and an error
  inside a present `Option` landed at the option's own key, which is
  not a key the form renders under, so the one thing the user needed
  to read rendered NOWHERE. The wording question is withdrawn — the
  form's "required" is the right word for a person and the switch was
  never worth its cost. `SIso` still hands its refinement whole to the
  decoder, which is correct (the refinement is the wrapper's, not a
  field's) and now says so in the code.
