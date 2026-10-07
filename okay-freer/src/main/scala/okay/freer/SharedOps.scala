package okay.freer

import okay.*


/**
 * The shared nodes of the fieldless operations (specs/effect-op-cost.md):
 * every `State.get` is this one `Inject(Get())`, every `Reader.ask` this
 * one `Inject(Ask())`, instead of a fresh pair per call.
 *
 * They live HERE and not in `State` / `Reader`: a `val` initialised with
 * an expression makes its module's initialisation impure to the inliner,
 * and every path through that module — `State.Get()` in a hand-staged
 * block, the stagers' own match on it — then stays a live call. Measured,
 * with the node inside `object State`: `StagedBenchmark.handBlock` built a
 * `Get` per operation it had dropped before (read in its bytecode).
 * Not for direct use; the staging macro reads both by name
 * (DirectRow's shared-node table).
 */
object SharedOps:
  val getNode: Any ! State % Any = effect(State.Get[Any, Any]())
  val askNode: Any ! Reader % Any = effect(Reader.Ask[Any, Any]())
  /** the threaded road's read, one node for every state type
   * (indexed-effects stage 1): `PState.Op.Get` has no fields either */
  val getT: Freer[PState.Op, Any, Any, Any] = Freer.Inject[PState.Op, Any, Any, Any](PState.Op.Get[Any]())
