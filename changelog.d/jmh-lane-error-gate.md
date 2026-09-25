## jmh-lane-error-gate - a JMH lane is accepted only when its rows are tight

`scripts/jmh-lane.sh` accepted a lane when the box was quiet at its
start and at its end. That cannot see a sibling's gate that starts and
ends inside the lane, and channel-default-adaptive (the same day) kept
rows of +-60% that passed both checks; its verdict held only after its
own driver added "accept at error <= 10% of the score".

Now jmh-lane.sh reads its run's result table, and a primary row (not a
`name:secondary` metric such as `:gc.count`, whose error may exceed its
score legitimately) with error above `JMH_LANE_MAX_ERR` percent of its
score (default 10; `0` turns the check off) makes the run contaminated:
discarded and retried, like a busy box, and the give-up message names
both causes. Checked against a real log that lane discarded: it flags
`default_elem 684.252 ± 169.125 (25%)` and skips JMH's `±(99.9%)`
Result line. Selftest cases 9-11 (noisy then tight, the switch off, a
noisy secondary metric ignored); case 9 red before, all pass under sh
and bash.
