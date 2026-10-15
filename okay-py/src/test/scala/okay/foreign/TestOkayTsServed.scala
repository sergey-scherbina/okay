package okay.foreign

import java.nio.file.{Files, Path, Paths}

import okay.codec.Schema
import okay.freer.{%, +, !, pure, given}
import okay.std.Writer

/**
 * okay-ts (github.com/sergey-scherbina/okay-ts), a far side that CONNECTS by
 * itself (foreign-server): its programs as data and generators, started by
 * this host over a connection its `ForeignServer` accepted. LIVE — node and an
 * okay-ts checkout (`OKAY_TS`, its directory) — and skipped where either is
 * absent.
 */
object OkayTs:
  private def has(cmd: String*) = scala.util.Try(ProcessBuilder(cmd*).start().waitFor() == 0).getOrElse(false)
  lazy val dir: Option[Path] = sys.env.get("OKAY_TS").map(Paths.get(_)).filter(d => Files.exists(d.resolve("src/index.ts")))
  lazy val ready: Boolean = dir.isDefined && has("node", "--version")

  /** a node process running `script` (its programs, connected to `port`) */
  def client(script: String, port: Int): Process =
    val d = Files.createTempDirectory("okay-ts-client")
    val f = d.resolve("client.ts")
    Files.writeString(f, script.replace("OKAY_TS", dir.get.resolve("src/index.ts").toString)): Unit
    val pb = ProcessBuilder("node", f.toString, port.toString)
    pb.redirectErrorStream(true)
    pb.redirectOutput(ProcessBuilder.Redirect.INHERIT)
    pb.start()

  /** the conformance programs, written in okay-ts: `pairs` a program of data (its rests resumed many times),
   * `total` a flatMap, `quote` a generator, `boom` a failure */
  val conformance: String = """
import { Do, connect, eff, effect, op, pure } from "OKAY_TS";

const Conf = effect("conf", {
  choose: op<[xs: number[]], number>(),
  price_of: op<[sku: string], number>(),
  discount: op<[amount: number], number>(),
});

await connect({ port: Number(process.argv[2]) }, {
  pairs: () => Do.bind("x", () => Conf.choose([1, 2])).bind("y", () => Conf.choose([10, 20])).map(({ x, y }) => x + y),
  total: (sku: string, qty: number) => Conf.price_of(sku).flatMap((p) => Conf.discount(p * qty)),
  quote: (sku: string, qty: number) => eff(function* () {
    const price = yield* Conf.price_of(sku);
    return yield* Conf.discount(price * qty);
  }),
  boom: () => pure(0).map(() => { throw new Error("typescript says no"); }),
}, { name: (o) => o.name });
"""

  /** payments: each transfer the server's — audited there; a refusal caught where the transfer is */
  val payments: String = """
import { connect, eff, effect, op } from "OKAY_TS";

const Pay = effect("Pay", { transfer: op<[t: { to: string; amount: number }], string>() }, { sensitive: true });

await connect({ port: Number(process.argv[2]) }, {
  checkout: (amounts: number[]) => eff(function* () {
    const receipts: string[] = [];
    for (const amount of amounts) {
      try {
        receipts.push(yield* Pay.transfer({ to: " acct-1234 ", amount }));
      } catch (e) {
        receipts.push(`${(e as Error).name}: ${(e as Error).message}`);
      }
    }
    return receipts;
  }),
}, { name: (o) => o.name });
"""

/** (okay-ts, ACCEPTED): the conformance suite, its far side okay-ts connecting to the server */
class TestOkayTsServed extends WireConformance:
  override def munitIgnore: Boolean = !OkayTs.ready
  override def tables: Boolean = false
  override def holds: Boolean = false
  private lazy val server = ForeignServer.listen()
  private lazy val client = OkayTs.client(OkayTs.conformance, server.port)
  lazy val engine: ForeignWorker =
    client: Unit
    server.accept()
  override def afterAll(): Unit = if OkayTs.ready then
    engine.close()
    client.destroy()
    server.close()

object TestOkayTsAudited:
  final case class Transfer(to: String, amount: Double) derives Schema

/** a TypeScript program's sensitive effect handled HERE: each transfer through the server's audit — the account
 * normalized, told to the trail, refused over the limit (a condition, caught in TypeScript where the transfer is) */
class TestOkayTsAudited extends okay.testkit.Munit.Diagnosed {
  import TestOkayTsAudited.Transfer

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override def munitIgnore: Boolean = !OkayTs.ready

  private given Shape = Shape.python
  private val limit = 1000.0

  /** the audit: a callback whose program tells the trail, and refuses — a condition the far side catches */
  private val transfer = Foreign.Callback[Writer % String]("transfer", args =>
    summon[Shape].decode[Transfer](args.head) match
      case Left(c) => pure[Writer % String, Either[Condition, Value]](Left(c))
      case Right(t) =>
        val account = t.to.trim.toUpperCase
        val masked = "****" + account.takeRight(4)
        if t.amount > limit then
          Writer.tell(s"refused: ${t.amount} to $masked over the limit $limit")
            .map(_ => Left(Condition("Refused", s"${t.amount} over the limit $limit")))
        else
          val receipt = s"ok:$account:${t.amount}"
          Writer.tell(s"transfer ${t.amount} to $masked → $receipt").map(_ => Right(summon[Shape].encode(receipt))))

  test("a TypeScript program's transfers audited by the server: told, normalized, refused over the limit") {
    val server = ForeignServer.listen()
    val client = OkayTs.client(OkayTs.payments, server.port)
    try
      val engine = server.accept()
      try
        given okay.Answers[ForeignEval] = engine.handler
        val checkout = Foreign.fn[Vector[String]]("checkout").calling(Foreign.callbacks(transfer))(Vector(100.0, 5000.0, 7.5))
        val (trail, receipts) = Writer.run(checkout).runWith
        note(trail.mkString("\n"))
        assertEquals(trail.toList, List(
          "transfer 100.0 to ****1234 → ok:ACCT-1234:100.0",
          "refused: 5000.0 to ****1234 over the limit 1000.0",
          "transfer 7.5 to ****1234 → ok:ACCT-1234:7.5"))
        assertEquals(receipts, Right(Vector("ok:ACCT-1234:100.0", "Refused: 5000.0 over the limit 1000.0", "ok:ACCT-1234:7.5")))
      finally engine.close()
    finally
      client.destroy()
      server.close()
  }
}
