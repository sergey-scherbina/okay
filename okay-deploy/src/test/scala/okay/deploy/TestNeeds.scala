package okay.deploy

import okay.{Module, module, wire}

/** the capabilities an application's root still waits for, each saying what it is to a deployment */
object Caps:
  trait Pg { def url: String }
  trait Files { def root: String }
  trait Unknown
  given Needs[Pg] = Needs(Need.Database(Engine.Postgres, "16", "shop"))
  given Needs[Files] = Needs(Need.Volume("/app/data"))

/** specs/di.md stage 3: the root module's inputs are the deployment's needs */
class TestNeeds extends munit.FunSuite:
  import Caps.*

  trait App { def ready: Boolean }
  type Root = Pg ?=> Files ?=> Module[[X] =>> App ?=> X]
  val root: Root = module[App](new App { val ready = wire[Pg].url.nonEmpty && wire[Files].root.nonEmpty })(_ => ())

  test("the needs are read off the root's type, one per unresolved input, in order") {
    assertEquals(Needs.of[Root], Vector(Need.Database(Engine.Postgres, "16", "shop"), Need.Volume("/app/data")))
  }

  test("a tupled input chain and a root with nothing left to resolve") {
    type Both = (Pg, Files) ?=> Module[[X] =>> App ?=> X]
    assertEquals(Needs.of[Both], Needs.of[Root])
    assertEquals(Needs.of[Module[[X] =>> App ?=> X]], Vector.empty)
  }

  test("a mixed root: the place's inputs are declared, the runtime's are dropped") {
    // a Timer is the process's own — declared `runtime` in Needs's
    // companion, for every application — so it must not appear as a
    // deployment need, and must not be an error either (needs-runtime)
    type Mixed = Caps.Pg ?=> okay.Timer ?=> Caps.Files ?=> Module[[X] =>> App ?=> X]
    assertEquals(Needs.of[Mixed], Vector(Need.Database(Engine.Postgres, "16", "shop"), Need.Volume("/app/data")))
  }

  test("an input a capability declares as the runtime's says nothing to the deployment") {
    trait Ticker
    given Needs[Ticker] = Needs.runtime
    assertEquals(Needs.of[Ticker ?=> Module[[X] =>> App ?=> X]], Vector.empty)
  }

  test("an input the deployment does not know is a compile error naming it") {
    val errs = compileErrors("Needs.of[Caps.Unknown ?=> okay.Module[[X] =>> Int ?=> X]]")
    assert(errs.contains("does not know what that is") && errs.contains("Unknown"), errs)
    // and the message offers both answers, not just the place's one
    assert(errs.contains("Needs.runtime"), errs)
  }

  test("a Service carries them beside the needs only the place can say — the database said once, in the type") {
    val web = Service("web", Run.Image("shop/web"), needs = Needs.of[Root] :+ Need.Port(8080))
    assertEquals(web.databases.map(_.database), Vector("shop"))
    assertEquals(web.volumes.map(_.path), Vector("/app/data"))
    assertEquals(web.mainPort, Some(8080))
  }
