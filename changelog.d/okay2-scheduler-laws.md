## okay2-scheduler-laws - the scheduler laws and soaks in okay2

The Scala 3 core's TestSchedulerLaws and TestAdaptiveScheduler, ported to
okay2-platform on the same `SchedulerFamily` (loom, drive, three `own`
configurations, adaptive). okay2's JVM schedulers are the core's class
for class, so there was no source change.

The laws cover joins, failures, `onComplete`, cancel races and `par`,
for every member. The soaks cover the defects the laws were written for:
- the Chase-Lev deque's conservation under thieves while it grows;
- the lost wakeup after a park;
- a blocked raw call hiding a fork or a child;
- the stuck-check waking a parked worker;
- fairness under a burst;
- `close`.

52 results (specs/okay2.md stage 37).
