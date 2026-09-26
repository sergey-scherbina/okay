package okay.py

/** the conformance programs in TypeScript: the same four as every other far side */
object TsConformance:
  val conf: String = """
import { done, okay_call, perform, then, type Prog } from "./okay.ts";

export function pairs(): Prog<number> {
  return then(perform<number>("choose", [1, 2]), (x) =>
    then(perform<number>("choose", [10, 20]), (y) => done(x + y)));
}

export function total(sku: string, qty: number): Prog<number> {
  return then(perform<number>("price_of", sku), (price) => perform<number>("discount", price * qty));
}

export function boom(): Prog<number> {
  throw new Error("typescript says no");
}

// DIRECT STYLE: ordinary TypeScript calling okay's effects
export function quote(sku: string, qty: number): number {
  const price = okay_call<number>("price_of", sku);
  return okay_call<number>("discount", price * qty);
}

// a TABLE call: the frame arrives as a record of columns
export function scale(frame: { x: number[] }, k: number): { x: number[] } {
  return { x: frame.x.map((v) => v * k) };
}
"""

  private def has(cmd: String*) = scala.util.Try(ProcessBuilder(cmd*).start().waitFor() == 0).getOrElse(false)
  lazy val node: Boolean = has("node", "--version")

  def start()(using WireFormat, WireCompression): ForeignWorker =
    val d = java.nio.file.Files.createTempDirectory("okay-ts-conf")
    java.nio.file.Files.writeString(d.resolve("conf.ts"), conf): Unit
    TsWorker.start(d, modules = Seq("conf"))

/** (TypeScript, pipes), JSON */
class TestTsPipes extends WireConformance:
  override def munitIgnore: Boolean = !TsConformance.node
  override def address(name: String): String = s"conf:$name"
  lazy val engine: ForeignWorker = TsConformance.start()
  override def afterAll(): Unit = if TsConformance.node then engine.close()

/** (TypeScript, pipes), CBOR and DEFLATE chosen by givens */
class TestTsPipesCbor extends WireConformance:
  import WireFormat.Cbor.given
  import WireCompression.Deflate.given
  override def munitIgnore: Boolean = !TsConformance.node
  override def address(name: String): String = s"conf:$name"
  lazy val engine: ForeignWorker = TsConformance.start()
  override def afterAll(): Unit = if TsConformance.node then engine.close()
