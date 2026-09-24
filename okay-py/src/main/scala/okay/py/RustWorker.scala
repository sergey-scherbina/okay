package okay.py

import java.nio.file.{Files, Path}
import okay.codec.Schema

/**
 * A Rust worker for okay (polyglot-one-wire, specs/polyglot-one-wire.md): a
 * Cargo crate depending on the `okay` crate this jar ships
 * (`/okay/rust-crate` — a directory name no package can have: a resource
 * directory `okay/rust/okay` on the classpath read to scalac as a PACKAGE
 * `okay.rust.okay`, and inside `package okay.rust` the name `okay` found it
 * instead of the root), serving programs as data and direct-style functions
 * with `okay::main(make)` — on stdin/stdout, or on TCP when `OKAY_LISTEN` is
 * set. `build` compiles it OFFLINE; `ForeignWorker.speaking` or `.connect`
 * reaches it, and `Foreign.program` / `Foreign.fn` drive it unchanged.
 */
object RustWorker:

  private def resource(path: String): String =
    val res = getClass.getResourceAsStream(s"/okay/rust-crate/$path")
    if res == null then throw IllegalStateException(s"okay.py: /okay/rust-crate/$path is missing from the jar")
    try String(res.readAllBytes(), java.nio.charset.StandardCharsets.UTF_8) finally res.close()

  /**
   * Compile the Cargo crate in `dir` against the shipped `okay` crate and
   * answer the binary. The `okay` crate is written to `dir/okay`; a
   * `Cargo.toml` naming the package `worker` and depending on it is written
   * if `dir` has none. `--offline`: the crate's one dependency, serde_json,
   * must already be in cargo's cache. A compile error refuses with rustc's
   * own words. `features` are the okay crate's own: `Seq("tls")` builds a
   * server that can speak TLS (wire-tls).
   */
  def build(dir: Path, cargo: String = "cargo", features: Seq[String] = Nil): Path =
    val lib = dir.resolve("okay")
    Files.createDirectories(lib.resolve("src")): Unit
    Files.writeString(lib.resolve("Cargo.toml"), resource("Cargo.toml")): Unit
    Files.writeString(lib.resolve("src").resolve("lib.rs"), resource("src/lib.rs")): Unit
    if !Files.exists(dir.resolve("Cargo.toml")) then
      Files.writeString(dir.resolve("Cargo.toml"),
        "[package]\nname = \"worker\"\nversion = \"0.1.0\"\nedition = \"2021\"\n\n[dependencies]\nokay = { path = \"okay\" }\n"): Unit
    val target = dir.resolve(".okay-build")
    val pb = ProcessBuilder((Vector(cargo, "build", "--offline", "--release", "--target-dir", target.toString) ++
        okayFeatures(features))*)
      .directory(dir.toFile).redirectErrorStream(true)
    val p =
      try pb.start()
      catch case e: java.io.IOException =>
        throw IllegalStateException(s"okay.py: '$cargo' did not start (${e.getMessage}) — is Rust installed?")
    val log = String(p.getInputStream.readAllBytes(), java.nio.charset.StandardCharsets.UTF_8)
    if p.waitFor() != 0 then
      throw IllegalStateException(s"okay.py: the Rust worker did not compile:\n${log.linesIterator.toVector.takeRight(25).mkString("\n")}")
    target.resolve("release").resolve("worker")

  /** the okay crate's features, as cargo names a dependency's */
  private def okayFeatures(features: Seq[String]): Vector[String] =
    if features.isEmpty then Vector.empty else Vector("--features", features.map(f => s"okay/$f").mkString(","))

  /**
   * Compile the crate in `dir` as a LIBRARY for use IN-PROCESS
   * (polyglot-one-wire stage 3): its `src/lib.rs` calls
   * `okay::export_worker!(make)`, and the answer is the `cdylib` for FFM
   * (`target` None) or the module for WebAssembly (`Some("wasm32-wasip1")`).
   * A `Cargo.toml` naming the package `worker` with `crate-type = ["cdylib"]`
   * is written if `dir` has none.
   */
  def buildLibrary(dir: Path, target: Option[String] = None, cargo: String = "cargo", features: Seq[String] = Nil): Path =
    val lib = dir.resolve("okay")
    Files.createDirectories(lib.resolve("src")): Unit
    Files.writeString(lib.resolve("Cargo.toml"), resource("Cargo.toml")): Unit
    Files.writeString(lib.resolve("src").resolve("lib.rs"), resource("src/lib.rs")): Unit
    if !Files.exists(dir.resolve("Cargo.toml")) then
      Files.writeString(dir.resolve("Cargo.toml"),
        "[package]\nname = \"worker\"\nversion = \"0.1.0\"\nedition = \"2021\"\n\n[lib]\ncrate-type = [\"cdylib\"]\n\n[dependencies]\nokay = { path = \"okay\" }\n"): Unit
    val out = dir.resolve(".okay-build")
    val cmd = Vector(cargo, "build", "--offline", "--release", "--target-dir", out.toString) ++
      target.toVector.flatMap(t => Vector("--target", t)) ++ okayFeatures(features)
    val p = ProcessBuilder(cmd*).directory(dir.toFile).redirectErrorStream(true).start()
    val log = String(p.getInputStream.readAllBytes(), java.nio.charset.StandardCharsets.UTF_8)
    if p.waitFor() != 0 then
      throw IllegalStateException(s"okay.py: the Rust library did not compile:\n${log.linesIterator.toVector.takeRight(25).mkString("\n")}")
    target match
      case Some(t) => out.resolve(t).resolve("release").resolve("worker.wasm")
      case None =>
        val os = System.getProperty("os.name").toLowerCase
        val file = if os.contains("mac") then "libworker.dylib" else if os.contains("win") then "worker.dll" else "libworker.so"
        out.resolve("release").resolve(file)

/**
 * Rust's side of typed operations: the operations a Rust program may
 * perform, written from the Scala callbacks that answer them, as `Go.ops`,
 * `Hs.ops` and `Ts.ops` write theirs. One function per operation, typed by
 * its argument and answer, for `okay::send` (programs) and `okay_call`
 * (direct style):
 *
 * {{{
 * pub fn price_of(a0: String) -> Op<f64> { Op::new("price_of", vec![a0.to_value()]) }
 * }}}
 */
object Rs:

  private def rustType(s: Schema[?]): Option[String] = s match
    case Schema.SInt | Schema.SLong => Some("i64")
    case Schema.SDouble => Some("f64")
    case Schema.SBool => Some("bool")
    case Schema.SString => Some("String")
    case l: Schema.SList[?] => rustType(l.of()).map(t => s"Vec<$t>")
    case v: Schema.SVector[?] => rustType(v.of()).map(t => s"Vec<$t>")
    case o: Schema.SOption[?] => rustType(o.of()).map(t => s"Option<$t>")
    case i: Schema.SIso[?, ?] => rustType(i.under())
    case _ => None

  private val keywords = Set("as", "break", "const", "continue", "crate", "else", "enum", "extern", "false", "fn",
    "for", "if", "impl", "in", "let", "loop", "match", "mod", "move", "mut", "pub", "ref", "return", "self", "static",
    "struct", "super", "trait", "true", "type", "unsafe", "use", "where", "while", "async", "await", "dyn")

  /** `price-of` -> `price_of`; a Rust keyword gets `r#` */
  def function(op: String): String =
    val n = op.replaceAll("[^A-Za-z0-9_]", "_")
    val lead = if n.headOption.exists(_.isDigit) then s"op_$n" else n
    if keywords(lead) then s"r#$lead" else lead

  /** the Rust module of typed operation constructors for these callbacks */
  def ops[F[+_]](cbs: Foreign.Callbacks[F]): String =
    val fns = cbs.all.map { c =>
      val (arg, res) = c.types match
        case Some((a, r)) => (rustType(a), rustType(r))
        case None => (None, None)
      val open = if arg.isEmpty || res.isEmpty then s" A type Rust reads as `Value`." else ""
      s"""/// the operation "${c.name}".$open
         |pub fn ${function(c.name)}(a0: ${arg.getOrElse("Value")}) -> Op<${res.getOrElse("Value")}> {
         |    Op::new("${c.name}", vec![a0.to_value()])
         |}""".stripMargin
    }
    s"""// Code generated by okay.py.Rs from the Scala callbacks. DO NOT EDIT.
       |#![allow(dead_code)]
       |
       |use okay::{Op, Value, Wire};
       |
       |${fns.mkString("\n\n")}
       |""".stripMargin
