package okay

/**
 * THE STRICT `k` BY RE-EXECUTION (cont-js-depth stage 4, specs/cont-js-depth.md): whether a strict body's `k` that
 * runs out of room suspends — the host stack unwound to the run's driver, the bodies on the way run again with
 * their `k`'s answers remembered — and the room: how many strict `k` calls deep a run nests before it does. On by
 * default where there is no second stack (Scala.js); `-Dokay.cont.replay=true` elsewhere. Variables so a test can
 * run the mechanism on any platform and with a small room.
 */
private[okay] object ContReplay:
  var on: Boolean = StackSwitch.replayByDefault
  /** strict `k` calls nested on the host stack before a suspension — well inside Scala.js's stack */
  var room: Int = 64
