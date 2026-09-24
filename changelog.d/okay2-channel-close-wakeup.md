## okay2-channel-close-wakeup - Growing no longer grows after its seal

TestChannelLaws law 1b hung twice under load. The receiver parked after
`close` and was never woken. The cause was in `Growing`, the default
buffer:
1. `close` sealed the end mark into the one-part ring.
2. A producer already past its open check then grew the ring into an
   `AdaptiveFifo` with a fresh, unsealed part.
3. The channel, which counts end marks per part, then waited for an end
   mark that nobody would ever place.

Sealing and growing are now one CAS-guarded decision
(specs/okay2.md stage 33). TestGrowingSeal reproduces the hang
deterministically: it failed before the fix and passes after it.
