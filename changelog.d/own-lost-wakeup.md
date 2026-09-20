## own-lost-wakeup - the non-Loom scheduler pick survives a fiber that blocks for good

The JDK 17 hangs jdk17-adaptive-runtime left open (`TestNio`,
`TestResumable`) were read as `Schedulers.own`'s pool being exhausted
by blocked accept/read fibers. The thread dump of the hung fork says
otherwise: fourteen workers, ONE in `accept()`, thirteen parked, the
client fiber waiting in a queue nobody would look at. `own` counts a
worker inside a task as awake, so a task that never returns keeps
every outside fork from waking a sleeper — a lost wakeup, from the
first test's single connection on.

`Schedulers.auto` picks `Schedulers.platform` where there is no Loom
now: `own` with its stuck-check on, every 5 ms — the mechanism
`adaptive` already had for a caller "not certain their fibers never
block", which a library default is. And the stuck-check wakes a
parked worker before it starts a new one; the first cut only grew,
and once the overflow was spent the next stall was the hang again.
`own.build` is unchanged. Three laws in `TestSchedulerLaws`, each red
on the old line for the reason it names; `TestNio` 6/6 and
`TestResumable` 4/4 on real JDK 17. BUGS.md `own-lost-wakeup`; the
backlog entry schedulers-own-hangs-under-blocking-nio is retired.
