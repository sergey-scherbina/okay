## dataflow-windowed-seek - the measurement box 2b asked for, and it chooses

specs/dataflow.md box 2b names two roads for letting a WINDOWED sink
seek and refuses to pick between them by taste: "measure the merge
traffic of the first against the replay length of the second before
choosing". Nobody had. Now somebody has, and the numbers decide it.

Over the cluster suites' own feed (200 000 events, sliding 1000/250,
lateness 300), in bytes:

    road A - open panes handed over EVERY epoch      3,334 B
    what an epoch costs today (closed panes)        15,683 B
    road B - horizon replay, per RESUME             16,368 B
    a windowed resume TODAY                      6,400,000 B

BOTH ROADS ARE AN ENORMOUS IMPROVEMENT ON TODAY - a windowed resume
replays the whole topic, 6.4 MB against road B's 16 KB, 390x.

AND BETWEEN THEM IT IS NOT CLOSE. Road A's traffic is paid every epoch
for the life of the run; road B's only when a session actually
resumes. Road A wins only above 4.91 resumes per epoch (39.69 at the
larger batch), and a system resuming five times per epoch has a
problem no sink design will fix.

THE CAVEAT POINTS AWAY FROM THE ROAD IT REJECTS, which is what makes
the verdict safe: road A is as cheap as it is only because ~69 panes
are open at a time on this feed, whose jitter is below the declared
lateness. Real disorder, or more keys, keeps far more open - making
road A worse while leaving road B's horizon roughly where it is.

WHY ARITHMETIC AND NOT A RUN: neither road exists, so there is nothing
to run. What the numbers price is the SHAPE of the feed - how many
panes are open when, and how far back the horizon sits - which is a
property of the window and the event times rather than of an
implementation nobody has written.

The box is now `[~]`: the measurement it demanded first is done and
the remaining work is road B alone.
