## native-runner-twelfth - the lost Native process, finally with the call it was in

The twelfth occurrence of `native-runner-error` (okayParseNative, on a
lane whose diff is one test file and prose). It is recorded like the
other eleven, and two things in it are new enough to be worth reading.

IT NAMES THE CALL IN FLIGHT. sbt's line is `(okayParseNative / Test /
loadedTestFrameworks)`, so the process was lost while sbt was LOADING
THE FRAMEWORKS — before any test of that module ran. Every earlier
reading said only that the module reported no tests, which is
consistent with a death anywhere in the run; this one places it at
startup. The settled cause has said for months that the binary's
connection ends while sbt still has a call in flight, and now there is
a specific call attached to it.

`scripts/gate.sh` DID NOT RECOGNISE THE SHAPE, and said so honestly:
"RED — a failure this script does not recognise". Both shapes it knows
carry an `Error: Total N, Failed 0, Errors 1` line from the module,
and this occurrence has none — the module never reported a total at
all. The ledger now says what the script would have to match instead,
together with the conditions it already demands and which held here:
zero `==> X` anywhere, 96 module totals and every one green, 5 547
results against 5 558 on the cold run and 5 559 on the unchanged
rerun.

Nothing is changed in the script. Teaching it a third signature is
still the same trade the entry has always named — a re-run that hides
a real compile or test failure is worse than a red gate — and the
decision belongs with whoever picks up `ci-native-flake`. What this
lane does is make sure they have the text.
