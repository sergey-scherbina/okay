## okay2-cross-platform - okay2's platform files for Scala.js and Scala Native

This is stage B of okay2-cross. okay2-async and okay2-platform are
crossProjects, and the platform uses the core's per-platform layout:
- **Native:** `CanBlock` is wait/notify, a timer is a thread, and
  `Schedulers.threads` and `pool` run fibers; `Task`, `TaskQueue`,
  `FiberCell` and a socket `Net` are included.
- **Scala.js:** the timer is `setTimeout` and the scheduler is the event
  loop; Node `net` (`NodeConn`) and the `Web` facades are included.
  There is NO `CanBlock`. `PlatformDefaults` is split, with
  `BlockingDefaults`, so a blocking join does not compile on JS, and a
  test pins that.

The callback suites run on all three platforms, and the blocking suite
on JVM and Native. The full okay2 gate from clean: 1685 results.

Found on the way (specs/okay2.md stage 32): the stream and stm JVM tests
now fork, after TestChannelLaws law 1b hung twice inside sbt's process.
That hang is filed as `okay2-channel-close-wakeup`. Test tasks are now
bounded at 6, after Native runners missed their 40 s connect window.

Docs: docs/okay2.md section 11.
