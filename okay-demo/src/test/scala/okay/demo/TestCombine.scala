package okay.demo

import okay.*
import okay.given
import java.time.Instant

/**
 * The exercise's own tests, in the two halves the implementation is
 * written in: the stage folded over a LIST — no scheduler, no clock,
 * nothing to make flaky — and then the same stage over two sources
 * that really do run concurrently, which is where the ordering the
 * exercise asks about actually happens.
 */
class TestCombine extends munit.FunSuite {

  import Combine.*

  private def at(s: String) = Timestamp(Instant.parse(s))
  private val t0 = at("2023-11-02T17:08:49Z")

  private val charging = Charging(t0, SocketId("s123"), VehicleId("v123"), PowerInWatts(60))

  private val battery = Seq(
    Battery(t0, VehicleId("v123"), StateOfChargeInPercent(50)),
    Battery(t0, VehicleId("v124"), StateOfChargeInPercent(49)),
    Battery(t0, VehicleId("v123"), StateOfChargeInPercent(51)),
  )

  private val enriched =
    Output(t0, SocketId("s123"), VehicleId("v123"), PowerInWatts(60), StateOfChargeInPercent(51))

  final case class MapStateRepo(repo: Map[VehicleId, StateOfChargeInPercent] = Map.empty)
    extends StateRepo:
    def get(vehicleId: VehicleId): Option[StateOfChargeInPercent] = repo.get(vehicleId)
    def put(vehicleId: VehicleId, value: StateOfChargeInPercent): StateRepo =
      copy(repo = repo.updated(vehicleId, value))

  private val emptyState = MapStateRepo()

  /** the pure run: no effects anywhere, so the answer is a value */
  private def fold(repo: StateRepo, events: Seq[Event]): (Seq[Output], StateRepo) =
    !.run(Writer.run(through(Writer.of(events.toList))(combine(repo))))

  /** a source that waits a while, then tells its elements */
  private def after[W](millis: Long)(ws: Seq[W]): Source[W] =
    !.widen[Unit, Async, Writer % W](Async.sleep(millis))
      .flatMap(_ => Source.of(ws.toList))

  test("the last state of charge is the one that is emitted") {
    val (out, _) = fold(emptyState, battery :+ charging)
    assertEquals(out, Seq(enriched))
  }

  test("output is emitted only when charging data arrives") {
    // three battery readings and no charging: nothing is due
    assertEquals(fold(emptyState, battery)._1, Seq.empty[Output])
    // and two charging readings emit twice off one battery reading
    val (out, _) = fold(emptyState, Seq(battery(0), charging, charging))
    assertEquals(out.length, 2)
    assert(out.forall(_.stateOfChargeInPercent == StateOfChargeInPercent(50)))
  }

  test("a vehicle with no state of charge yet emits nothing") {
    val (out, _) = fold(emptyState, Seq(charging, battery(0), charging))
    assertEquals(out, Seq(Output(t0, SocketId("s123"), VehicleId("v123"),
      PowerInWatts(60), StateOfChargeInPercent(50))))
  }

  test("the state each vehicle carries is its own") {
    val other = Charging(t0, SocketId("s124"), VehicleId("v124"), PowerInWatts(11))
    val (out, _) = fold(emptyState, battery ++ Seq(charging, other))
    assertEquals(out.map(_.stateOfChargeInPercent),
      Seq(StateOfChargeInPercent(51), StateOfChargeInPercent(49)))
  }

  test("the answer is the repository the run ended with") {
    val (_, repo) = fold(emptyState, battery)
    assertEquals(repo.get(VehicleId("v123")), Some(StateOfChargeInPercent(51)))
    assertEquals(repo.get(VehicleId("v124")), Some(StateOfChargeInPercent(49)))
  }

  // ---- the same join, written as fs2 writes it ----

  /** the accumulating formulation, run the same way */
  private def foldAccumulating(repo: StateRepo, events: Seq[Event]): Seq[Option[Output]] =
    !.run(Writer.run(through(Writer.of(events.toList))(accumulating(repo))))._1

  test("the two formulations agree, event for event") {
    val cases = Seq(
      battery :+ charging,
      Seq(charging, battery(0), charging),
      battery ++ Seq(charging, Charging(t0, SocketId("s124"), VehicleId("v124"), PowerInWatts(11))),
      Seq(charging),
      battery,
    )
    for events <- cases do
      val direct = fold(emptyState, events)._1
      val viaAccumulate = foldAccumulating(emptyState, events).flatten
      assertEquals(viaAccumulate, direct, s"disagreed on $events")
      // and through the filtering stage, the accumulating form is the
      // same pipeline as the direct one — two stages instead of one
      val piped = !.run(Writer.run(through(Writer.of(events.toList))(accumulated(emptyState))))._1
      assertEquals(piped, direct)
  }

  test("what the 1:1 contract costs: an Option per event") {
    val events = battery :+ charging          // three batteries, one charging
    val told = foldAccumulating(emptyState, events)
    assertEquals(told.length, 4)              // one output per INPUT, necessarily
    assertEquals(told.count(_.isEmpty), 3)    // three Nones nobody asked for
    assertEquals(fold(emptyState, events)._1.length, 1)   // the direct form tells once
  }

  // ---- the same stage, over two sources merged by readiness ----

  test("merged: the battery arrives first, so the charging is enriched") {
    val (out, _) = outputs(emptyState, Source.of(battery.toList), after(100)(Seq(charging)))
    assertEquals(out, Seq(enriched))
  }

  test("merged: the charging arrives first, so there is nothing to enrich") {
    val (out, _) = outputs(emptyState, after(100)(battery), Source(charging))
    assertEquals(out, Seq.empty[Output])
  }

  test("merged: a seeded repository gives the charging a default") {
    val seeded = MapStateRepo(Map(VehicleId("v123") -> StateOfChargeInPercent(0)))
    val (out, _) = outputs(seeded, after(100)(battery), Source(charging))
    assertEquals(out, Seq(Output(t0, SocketId("s123"), VehicleId("v123"),
      PowerInWatts(60), StateOfChargeInPercent(0))))
  }

  test("merged: both sources drain, whatever the interleaving") {
    val (out, repo) = outputs(emptyState, Source.of(battery.toList), Source(charging))
    assertEquals(repo.get(VehicleId("v124")), Some(StateOfChargeInPercent(49)))
    assert(out.length <= 1, out.toString)
  }
}
