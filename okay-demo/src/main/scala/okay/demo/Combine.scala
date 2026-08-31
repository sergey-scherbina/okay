package okay.demo

import okay.*
import okay.given
import okay.!.widen

import java.time.Instant

/**
 * The stream-joining exercise, in okay: two live sources — charging
 * telemetry and battery telemetry — merged by READINESS, and every
 * charging reading enriched with the last state of charge seen for
 * that vehicle. (The fs2 original is ../exercise/stream-exercise:
 * `battery.map(Left).merge(charging.map(Right)).mapAccumulate(repo)`.)
 *
 * It is written in the two halves the library separates, and that is
 * the whole point of having it here:
 *
 *   - `combine` is a PURE Stage — awaits events, tells outputs, and
 *     its state is a recursion parameter, not a cell and not a
 *     `mapAccumulate`. It has no effects at all, so it is tested
 *     against a list: no scheduler, no clock, no waiting.
 *   - `outputs` is the concurrent half: `mergeSources` runs a fiber
 *     per source and interleaves them by whoever is ready, and the
 *     SAME stage is then run over the merged source by `through`.
 *
 * Nothing between them is materialized: the consumer's demand pulls
 * one event through the merge at a time.
 */
object Combine {

  // the wrappers of the exercise, for correctness at the call site
  final case class Timestamp(value: Instant)
  final case class VehicleId(value: String)
  final case class SocketId(value: String)
  final case class StateOfChargeInPercent(value: Int)
  final case class PowerInWatts(value: Int)

  final case class Battery(timestamp: Timestamp,
                           vehicleId: VehicleId,
                           stateOfChargeInPercent: StateOfChargeInPercent)

  final case class Charging(timestamp: Timestamp,
                            socketId: SocketId,
                            vehicleId: VehicleId,
                            powerInWatts: PowerInWatts)

  final case class Output(timestamp: Timestamp,
                          socketId: SocketId,
                          vehicleId: VehicleId,
                          powerInWatts: PowerInWatts,
                          stateOfChargeInPercent: StateOfChargeInPercent)

  /**
   * What the merged stream carries: a UNION, where the fs2 version
   * needs an `Either` — `mergeSources` tells the union of its two
   * sources, so there is no wrapper to put on and none to take off.
   * The stage below splits it by an ordinary type test.
   */
  type Event = Battery | Charging

  /** the last state of charge per vehicle (the exercise's interface,
   * unchanged: persistent, so the stage stays pure) */
  trait StateRepo:
    def get(vehicleId: VehicleId): Option[StateOfChargeInPercent]
    def put(vehicleId: VehicleId, value: StateOfChargeInPercent): StateRepo

  /**
   * The join itself: await an event, tell an output when one is due,
   * answer the final repository.
   *
   * Battery readings only update the state; a charging reading emits
   * — and emits NOTHING when that vehicle has no state of charge yet,
   * which is the exercise's "only emit when charging data arrives"
   * plus "the last received state of charge". The state is the
   * recursion's parameter, so there is nothing to reset between runs
   * and nothing to hide a mutation in.
   */
  def combine(repo: StateRepo): Stage[Event, Output, StateRepo] =
    Stage.await[Event, Output].flatMap {
      case None => pure(repo)
      case Some(b: Battery) =>
        combine(repo.put(b.vehicleId, b.stateOfChargeInPercent))
      case Some(c: Charging) => repo.get(c.vehicleId) match
        case None => combine(repo)
        case Some(soc) =>
          Stage.tell[Event, Output](Output(
            timestamp = c.timestamp,
            socketId = c.socketId,
            vehicleId = c.vehicleId,
            powerInWatts = c.powerInWatts,
            stateOfChargeInPercent = soc)).flatMap(_ => combine(repo))
    }

  /**
   * The concurrent half: the two sources merged by READINESS (a fiber
   * per source, the loser of every race simply arrives later), then
   * the pure stage run over the merge. `widen` embeds the stage in
   * the wider row — Free is invariant in its signature, so that
   * embedding is a walk rather than a subtyping step. Lazy
   * throughout: the fold at the end is what pulls.
   */
  def joined(repo: StateRepo, battery: Source[Battery], charging: Source[Charging])
            (using Scheduler, CanBlock): StateRepo ! (Writer % Output + Async) =
    through[Event, Output, Async, Unit, StateRepo](mergeSources(battery, charging))(
      widen[StateRepo, Take % Event + Writer % Output, Async](combine(repo)))

  /** the outputs, and the repository the run ended with */
  def outputs(repo: StateRepo, battery: Source[Battery], charging: Source[Charging])
             (using Scheduler, CanBlock): (Seq[Output], StateRepo) =
    Writer.run[Output, StateRepo, Async](joined(repo, battery, charging)).runWith
}
