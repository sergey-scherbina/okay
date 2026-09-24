- [ ] okay2-channel-close-wakeup — okay2-stream's TestChannelLaws law 1b
      (close racing four non-parking offers) HUNG once in a full okay2
      gate on 2026-09-24 (okay2-cross stage B, 1483 results in, JS and
      Native runners busy on the same box). The gate's dump, sbt's JVM:
      the law's receiver a virtual thread WAITING in
      `LockSupport.park <- Platform.await (Platform.scala:70) <-
      Channel.receiveBlocking (Channel.scala:114) <-
      SentinelChannel.receiveBlocking (SentinelChannel.scala:34) <-
      TestChannelLaws.scala:120`, the test thread in `Thread.join`
      at TestChannelLaws.scala:127, nothing at CPU — a receiver parked
      AFTER close and never woken, which reads as a lost wakeup between
      close and a parked receiver rather than starvation. The suite
      alone was green (104 results) the same minute. Next: run law 1b
      under burners (a busy box is what surfaced it) until it hangs,
      dump from inside the wait (hang-look-for-the-missing-answerer),
      and read SentinelChannel's close path against its park. (2026-09-24)
