## adaptive-seal-race - a closed channel could lose its END under load, in the default channel

Two races, one shape, in okay-stream and okay2-stream alike: a part of
a partitioned buffer could exist without an end mark, so the consumer
met fewer end marks than parts and parked for good on a closed, drained
channel (`finished` already true).

- `AdaptiveFifo`: the freeze was a flag `claimPart` read BEFORE bumping
  `open`, so a producer could pass the check, `seal` freeze and count,
  and the producer then open part n unsealed. Now the freeze is a bit of
  `open` itself, set by CAS; a part opens by CAS only while unfrozen,
  and `seal` waits for the slot of every part its count covers.
- `Growing`: a swap could install its partitioned buffer AFTER the ring
  took its end mark. In okay2 this half was found and fixed the same
  evening by okay2-channel-close-wakeup (0f0e5977, one `AtomicReference`
  state: a seal that wins over the open ring rules growth out); this lane
  carries that design to okay-stream's `Growing`, where it is the
  DEFAULT channel's buffer.

Found by law 1b (channel-law-racing-offers, the same day) hanging a
loaded okay2 gate; reproduced by six law-1b loops at once — okay2's
adaptive and growing lost the end in 4 of 6 runners within 800 rounds,
the Scala 3 DEFAULT (`Channel.apply`, growing) in 4 of 6 within 66.
That shape is law 1c in both `TestChannelLaws`, failing in five
seconds instead of hanging a gate; red on the old code in both cores,
green on the fix (repeated). Laws 1b and 1c now bound each producer to
50 000 offers per round: unbounded, law 1b timed out at 30 s under a
full gate on SentinelChannel/unbounded.
