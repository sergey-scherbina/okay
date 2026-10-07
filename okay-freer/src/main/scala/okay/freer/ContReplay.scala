package okay.freer


/**
 * THE STRICT `k` BY RE-EXECUTION (cont-js-depth stages 4-5, specs/cont-js-depth.md): whether a strict body's `k` that
 * runs out of room suspends — the host stack unwound to the run's driver, the bodies on the way run again with
 * their `k`'s answers remembered — and the room: how many strict `k` calls deep a run nests before it does. Set by
 * the run-time mode (`Cps.Mode`, `-Dokay.cont.mode=auto|replay|safe`): `Auto` re-executes where there is no second
 * stack (Scala.js). `on` and `room` are variables so a test can run the mechanism on any platform, small.
 */
private[okay] object ContReplay:
  private var current: Cps.Mode = initial
  /** re-execution, as `current` resolves on this platform: read once a strict call */
  var on: Boolean = resolve(current)
  /** strict `k` calls nested on the host stack before a suspension — well inside Scala.js's stack */
  var room: Int = 64

  def mode: Cps.Mode = current
  def set(m: Cps.Mode): Unit = { current = m; on = resolve(m) }

  private def resolve(m: Cps.Mode): Boolean = m match
    case Cps.Mode.Auto => StackSwitch.replayByDefault
    case Cps.Mode.Replay => true
    case Cps.Mode.Safe => false

  /** `-Dokay.cont.mode`, and stage 4's `-Dokay.cont.replay=true`, which it replaces */
  private def initial: Cps.Mode =
    System.getProperty("okay.cont.mode") match
      case "replay" => Cps.Mode.Replay
      case "safe" => Cps.Mode.Safe
      case _ => if System.getProperty("okay.cont.replay") == "true" then Cps.Mode.Replay else Cps.Mode.Auto
