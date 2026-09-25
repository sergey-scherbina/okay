## stacked-shift0 - shift0 and dollar in Delim.Stacked; a capture's body gets the stack it really has

specs/shift0-dollar.md stage 2.

- Found first: the stacked `shift` typed its body under the WHOLE
  prompt stack. A shift from the body to an inner prompt the capture
  had just taken compiled, then threw `NoPrompt`. That is the failure
  `Delim.Stacked` exists to rule out, and it is now a compile error.
- `Has.Aux[S, P, B]` carries the stack below the prompt, found by the
  same induction as the prompt itself (a match type cannot reduce over
  prompt singletons). `shift`/`control` bodies get the stack `p *: B`,
  and `shift0` bodies get `B` (ICFP 2011's rule), each as a given of
  its own. `dollar` is stacked, and `control0` is deliberately not.
- TestStackedShift0 (8): the closed hole, bodies reaching prompts below
  (22, 202), shift0 at the root, the consumed prompt refused, stacked
  dollar with R0 ≠ R, and dollar's prompt gone after it returns. It also
  pins one CONSERVATIVE refusal: ICFP 2011's own example, which calls a
  continuation where its prompt is consumed. A mutant `Below` was
  watched failing.
- Updated records this lane made false: specs/freer-base.md ("shift0
  not stacked"), specs/delimited-control.md (variants out of scope), and
  docs/continuations-in-practice.md, now with ICFP 2011.
