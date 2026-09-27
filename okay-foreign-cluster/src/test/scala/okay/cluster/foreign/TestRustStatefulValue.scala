package okay.cluster.foreign

import okay.given
import okay.cluster.{Cluster, Jobs}
import okay.foreign.RustWorker
import java.nio.file.Files

/** stage 5 for a COMPILED worker: the stateful stage Rust could not have
 * under Decision 23 (a held value is never changed in place) — the state
 * is a value the function answers, so nothing is held at all */
object RustValued:
  private val main = """use okay::{function, Functions, Programs, Value, Wire, Worker};

fn column<'a>(cols: &'a [(String, Vec<Value>)], name: &str) -> Result<&'a Vec<Value>, String> {
    cols.iter().find(|(n, _)| n == name).map(|(_, v)| v).ok_or_else(|| format!("no column '{}'", name))
}

fn make() -> Worker {
    let mut functions = Functions::new();
    // open(params) -> state: the running sum starts at params.by
    functions.insert("vopen".into(), function(|args| match &args[0] {
        Value::Dict(kv) => kv.iter().find(|(n, _)| n == "by").map(|(_, v)| v.clone()).ok_or_else(|| "no by".to_string()),
        _ => Err("params: not a dict".to_string()),
    }));
    // step(frame, state) -> {rows: frame, state}: the rows with their running sums, and the sum after them
    functions.insert("vstep".into(), function(|args| {
        let cols = match &args[0] { Value::Table(c) => c, _ => return Err("step: not a table".to_string()) };
        let mut run = i64::from_value(&args[1])?;
        let keys = column(cols, "key")?;
        let vs = column(cols, "v")?;
        let mut runs = Vec::with_capacity(vs.len());
        for v in vs { run += i64::from_value(v)?; runs.push(Value::Int(run)); }
        let rows = Value::Table(vec![("key".into(), keys.clone()), ("v".into(), vs.clone()), ("run".into(), runs)]);
        Ok(Value::Dict(vec![("rows".into(), rows), ("state".into(), Value::Int(run))]))
    }));
    // finish(state) -> frame: one row of -1 with the total
    functions.insert("vfinish".into(), function(|args| {
        let run = i64::from_value(&args[0])?;
        Ok(Value::Table(vec![("key".into(), vec![Value::Int(-1)]), ("v".into(), vec![Value::Int(0)]), ("run".into(), vec![Value::Int(run)])]))
    }));
    Worker::new(Programs::new(), functions)
}

fn main() {
    okay::main(make)
}
"""
  lazy val module: WorkerModule =
    val d = Files.createTempDirectory("okay-rust-valued")
    Files.createDirectories(d.resolve("src")): Unit
    Files.writeString(d.resolve("src").resolve("main.rs"), main): Unit
    WorkerModule("rust", "rustvalued", Seq(RustWorker.build(d).toString))

/** the same `ValuedRunningJob` text, handed a `WorkerModule` (Live, needs cargo) */
class TestRustStatefulValue extends munit.FunSuite:
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override def munitIgnore: Boolean = !MoreLanguages.cargo
  override val munitTimeout = scala.concurrent.duration.Duration(10, "min")

  test("a functional stateful stage in a Rust worker: the state a value the JVM carries, the running sums and the finals exact over three workers") {
    val running = ValuedRunningJob("test.valued.rust", RustValued.module)
    Jobs.register(running)
    assertEquals(Cluster.run(running, Scale(10000), 4, Vector.fill(3)(Cluster.local)).runWith.value,
      StatefulJobs.expected(10000, 4))
  }
