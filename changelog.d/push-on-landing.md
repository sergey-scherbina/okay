## push-on-landing - pushing is the last step of landing, not an errand to ask about

The operator's rule, 2026-09-18: **push what you land, immediately,
without asking.** `git push origin master` is the last step of landing
a lane, after the release-claim commit.

WHAT THE OLD RULE COST. It said pushing was "a SEPARATE, deliberate act
by whoever the operator asks", and the ledger of that is in this file's
own history: `origin` 60 commits behind on 2026-09-08, 16 behind on
2026-09-18, and a submodule consumer blocked both times — okay-watch's
rule is that its pointer must name a commit that EXISTS on GitHub, so
an unpushed okay is a product that cannot bump. Twice today a finished
piece of work sat waiting for a yes that was always going to be yes.
Asking cost the operator an interruption per lane and bought nothing.

WHAT DOES NOT CHANGE, and it is the part worth saying out loud: the
gate still runs before the merge, the merge is still its own command
whose exit code you read, and you push what is already LANDED and
green — never a branch, never a lane that has not merged. A push that
is rejected means a sibling pushed first: fetch, integrate by the rule
below it, push again. Never force.

ONE SENTENCE IN THE NEXT BULLET WAS NOW FALSE and is corrected rather
than left: "NOTHING in the landing procedure pushes" was the reason
given for never `merge --ff-only origin/master`. The prohibition
stands, and the reason is now a smaller window rather than an open one
— seconds between a sibling's merge and their push — which still
discards a whole lane if you fast-forward inside it, and still looks
like nothing happened.

okay-watch carries the same rule in its own AGENTS.md (their e626391),
because a product that waits for permission to push is a product whose
submodule pointer cannot be published.
