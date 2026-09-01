# 9. Conditions: resumable exceptions

## The idea, and where it comes from

An exception, as most languages ship it, destroys the very thing a
recovery needs: by the time a handler runs, the stack between the
raise and the handler is gone, and "continue from where you were,
with this value" is no longer expressible. The Lisp family knew
better for decades. Smalltalk-80's exceptions could answer `resume:`
\[[Goldberg & Robson 1983](#ref-goldberg-1983)\]; Dylan carried the
design forward \[[Shalit 1996](#ref-shalit-1996)\]; and Common Lisp's
condition system — standardized in ANSI CL and given its definitive
history and rationale by Kent Pitman \[[Steele 1990](#ref-steele-1990);
[Pitman 2001](#ref-pitman-2001)\] — separated the three roles that
one `catch` conflates:

- **signaling** raises a condition *without unwinding* — the signal
  point's continuation stays live;
- **handling** is a *policy*: code, possibly far away, that sees the
  condition and decides;
- **restarts** are named recovery frames established *between* the
  two, each knowing how to continue from *its* place — and the
  policy chooses from the menu it finds.

The decision space this opens — resume with a value, unwind to a
chosen frame, or decline — is what an operator mid-incident actually
needs, and it is why R, a language for long-running data jobs, kept
the full design \[[R's condition system, Pitman-derived](#ref-pitman-2001)\].

## Why Okay gets it almost for free

Chapter 5's slogan — a handler answers an operation and the
continuation continues — *is* resumability. Plotkin and Pretnar's
handlers were introduced, in so many words, as a generalization of
exception handlers where the handler may resume
\[[Plotkin & Pretnar 2009](#ref-plotkin-2009), [2013](#ref-plotkin-2013)\];
the equivalence between conditions-with-restarts and delimited
control is folklore made precise by that line of work. So in Okay
(`Condition.scala`, [specs/condition.md](../../specs/condition.md))
the system is small by construction:

- `signal` is an *operation* (`Condition.scala:54`) — resumption is
  not new machinery, it is what every operation already does;
- a *restart* is a prompt in the Delim discipline (chapter 2):
  `within` installs a named frame (`Condition.scala:62`), and
  invoking it is an abort to that frame with a value — the machine
  (`Condition.scala:76`) owns the frame stack and the menu exactly
  as `Delim.run` owns its prompts;
- the *policy* is the handler at the boundary
  (`Condition.run(policy)(prog)`), receiving the condition and the
  menu, answering `Resume`/`Invoke`/`Fail`.

The typing honesty is stated in the file header rather than hidden:
the operation payloads are programs in the same row, which a
single-parameter signature cannot express, so they are erased at the
operation and re-typed inside the one machine that owns the frames —
the same sealed-invariant discipline as `Delim` and `Writer`.

## Direct style completes the picture

Chapter 8's reflection makes the Lisp reading *literal*. In a direct
block, `signal[Int]("how many?").?` is a call that may return: the
mark captures the rest of the block as the continuation, the policy
answers `Resume(41)`, and execution proceeds from the mark with 41 —
`TestConditionDirect` asserts the "before" work survives and the
"after" work sees the value. A restart frame takes a direct body
through the `frame` door, and the operator's story — repair a
malformed element and continue the batch — is a for-do loop with a
signal in its body, resumed per element.

The modern theory of exactly this two-way control flow is Zhang,
Salvaneschi and Myers' *bidirectional algebraic effects*
\[[Zhang, Salvaneschi & Myers 2020](#ref-zhang-2020)\]: an operation
travels up to its handler, the resumption travels back down, and
their type system tracks both directions — the condition/decision
pair (`Signal` up, `Decision` down) is a bidirectional effect in
their sense. Their soundness concerns also name Okay's recorded
roads precisely: today `signal[A]` erases the condition/answer pair
(a policy that resumes with the wrong type is a cast failure at the
signal point), and `Invoke` of an absent restart is a runtime
`NoSuchRestart`. The BACKLOG roads — typed condition/answer pairs,
and lexical restarts as capabilities in the ctx-prompts pattern
(chapter 8's gates applied to frames, making a nonexistent restart
uncompilable in scope) — are the two places where the Zhang–Myers
discipline would tighten this system, and they wait, as everything
here waits, for a consumer.

## References

- <a id="ref-goldberg-1983"></a>Adele Goldberg, David Robson.
  *Smalltalk-80: The Language and its Implementation.*
  Addison-Wesley, 1983 — resumable exceptions in the signal/`resume:`
  protocol.
- <a id="ref-steele-1990"></a>Guy L. Steele Jr.
  *Common Lisp the Language*, 2nd edition. Digital Press, 1990 —
  the condition system as standardized.
- <a id="ref-shalit-1996"></a>Andrew Shalit.
  *The Dylan Reference Manual.* Addison-Wesley, 1996 — the
  condition/restart design outside Lisp syntax.
- <a id="ref-pitman-2001"></a>Kent M. Pitman.
  *Condition handling in the Lisp language family.* In *Advances in
  Exception Handling Techniques*, LNCS 2022, 2001 — the history,
  the rationale, and the separation of signaling, handling and
  restarting.
- <a id="ref-plotkin-2009"></a>Gordon Plotkin, Matija Pretnar.
  *Handlers of algebraic effects.* ESOP 2009.
- <a id="ref-plotkin-2013"></a>Gordon Plotkin, Matija Pretnar.
  *Handling algebraic effects.* Logical Methods in Computer Science
  9(4), 2013 — handlers as resumable exception handlers, made
  precise.
- <a id="ref-zhang-2020"></a>Yizhou Zhang, Guido Salvaneschi,
  Andrew C. Myers. *Handling bidirectional control flow.*
  OOPSLA 2020 — the typed account of signal-up/resume-down.
