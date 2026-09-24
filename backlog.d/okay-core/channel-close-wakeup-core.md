- [ ] channel-close-wakeup-core — the CORE twin of okay2's
      `okay2-channel-close-wakeup`: okay-stream's
      `okay.TestChannelLaws` "law: an accepted element is delivered when
      close races many offers — SentinelChannel/unbounded" timed out (30 s)
      in affected-separate-builds' full gate on 2026-09-24, at load 26 with
      the whole matrix running. The suite alone was green (104 results)
      minutes later. okay2's gate hung on the same law the same day, and
      its dump showed a receiver parked in `SentinelChannel.receiveBlocking`
      after close, never woken. The cause is most likely SHARED (okay2 is a
      port of the core), so a fix found in okay2 must be checked here too,
      and the other way round. Reproduce under burners (20+ busy loops,
      repeated runs of this one law) and dump from inside the wait
      (hang-look-for-the-missing-answerer). (2026-09-24)
      SECOND SIGHTING (wire-compression-measured's gate, 2026-09-24 ~22:55,
      load ~45): the same law, the same channel, 30 s timeout — AFTER
      adaptive-seal-race (3b40bd4e) landed, so that fix (AdaptiveFifo,
      Growing) does not cover SentinelChannel/unbounded. Alone again
      green (111 results) at once.
