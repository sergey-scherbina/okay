## stackbytes-why-256 — the probe was measuring the host, not only the door

`stackbytes-json-read-not-flat-on-aarch64` closed wontfix and rightly:
green on the operator's machine, so `okay-codec` was never at fault.
What it left open was why a container read 256 KB at all. Measured,
not closed twice — three causes, every one in the measurement:

- **a thread's stackSize is a request.** With `StackShadowPages=20`
  and 4 KB pages, requests of 8, 16, 24, 32, 48, 64, 96 and 128 KB all
  gave the SAME 876 frames — ~34 KB usable, 80 KB of shadow inside a
  ~128 KB minimum — while 160/192/224/256 gave 1490/2308/3128/3948. A
  ladder of powers of two has one rung below 256 there, so a door
  needing a little over the floor reports 256, an 8x overstatement,
  and a door sitting on the floor flaps between rungs;
- **the cold round answers a different question.** Cold, depth 8
  answered 256, 16, 16, 16, 16 across five rounds; warm, depths 8,
  100, 200 and 400 answered 16 every round;
- **and the comparison was wrong.** 8 levels is BELOW
  `Codecs.NativeThreshold` and 100 is past it, so the test compared
  the native path against the trampolined one and called the
  difference a failure to flatten. `Cbor.read[Tree]` wants 256 KB at
  8 levels and 16 at 100 and at 400: that IS the trampoline working.

So the test now asks what the two trampoline lanes actually promised —
past the threshold the cost stops following the depth — comparing two
depths BOTH past it, on warm doors, and adds `deep <= shallow` so the
trampoline may never cost more than the recursion it replaced. The
safety lane keeps its cold max-of-3, because there a flap is the
answer you must keep.

216 green in okay-codec, and the line to read is
`Json.read[Tree] 16 KB at 8 levels, 16 KB at 100, 16 KB at 400`.
