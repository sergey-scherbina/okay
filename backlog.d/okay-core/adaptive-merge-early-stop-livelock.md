- [ ] adaptive-merge-early-stop-livelock — with `adaptive` as the given
      (`-Dokay.scheduler=adaptive` in okay-stream's test fork),
      TestReadyMerge never finishes: TWO runs on master 9ec45ca40, 32 and
      4 minutes, no test result, one core busy. On Loom the same tree reads
      29/29 green (scheduler-default-rerun, 2026-09-28). It is the one
      correctness red that kept the default on Loom.
      WHERE (two `jcmd Thread.print` 6 s apart): the given's worker
      `okay-own-1-0` had used 243 s of CPU in 247 s. The main thread was
      parked in `DriveTask.joinEither` at TestReadyMerge.scala:239, the
      early-stop test's first arm (`Source.of(LazyList.from(0)).merge(...,
      capacity = 4)` then `runFoldUntil(FoldUntil.take(5))`, forked on
      `summon[Scheduler]`). The other 13 workers were parked at ~0 CPU,
      and the monitor had used 7 s. The worker's stack, the same in both
      dumps:
        ConcurrentLinkedQueue.remove  <- SentinelChannel.attemptSend:351
        <- sendAsync <- Channel.send <- Drive.op ... <- SentinelChannel
        .attemptSend:345 <- wakeOne:180 <- wakeSender:97 <-
        receiveManyNow:441 <- Channel$package:529 <- Drive.op
      :351 is the `else` branch. `sendersAt(route)` is not empty, so the
      send enqueues its waiter. `hasRoomAt` is true and the claim wins, so
      it removes the waiter and loops. That repeats for as long as another
      waiter sits at the head of the queue. It is a busy-wait for some
      OTHER fiber to make progress. On Loom that fiber runs on another
      carrier. On `adaptive` it evidently never runs: the sender was
      resumed INLINE by the receiver (wakeSender -> attemptSend on the
      receiver's thread), and the monitor did not spread whatever would
      end the wait. That last part is a hypothesis: the dump does not name
      the fiber the loop waits for.
      THE LANE: (1) a law that reproduces it on `Schedulers.adaptive.build`
      directly (not via the property), red first, bounded by a timeout;
      (2) find the waiter at the head and who should wake it; (3) fix the
      loop so it cannot spin without progress (park instead of retrying,
      or never retry past a head that is not ours); (4) TestReadyMerge
      green under `-Dokay.scheduler=adaptive`; then the default question
      can be re-asked (specs/schedulers.md, "The default, re-run": every
      performance row already allows the flip).
