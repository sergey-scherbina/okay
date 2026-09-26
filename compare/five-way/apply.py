#!/usr/bin/env python3
"""Add okay as a sixth runtime to a LOCAL clone of
github.com/stasimus/scala-effect-bench at 82ac6f1.

    python3 compare/five-way/apply.py <path-to-clone>

Nothing is committed or pushed anywhere: the clone is edited in place so
okay runs in the same harness, with the same workloads and JMH settings,
beside CE, Kyo, Loom, Ox and Gears. Every replacement asserts its
anchor, so a changed upstream fails loudly instead of half-applying.

okay is compiled with Scala 3.9, so the clone's scalaVersion moves from
3.8.4 to 3.9.0 for every runtime alike; publish okay first with
`scripts/gate.sh "okayJVM/publishLocal; okayAsyncJVM/publishLocal;
okayPlatformJVM/publishLocal"`.
"""
import pathlib
import shutil
import sys

HERE = pathlib.Path(__file__).resolve().parent
OKAY = '"okay", "okayOwn"'


def edit(path, pairs):
    text = path.read_text()
    for old, new in pairs:
        if text.count(old) != 1:
            sys.exit(f"{path}: anchor not found exactly once: {old!r}")
        text = text.replace(old, new)
    path.write_text(text)


def main():
    root = pathlib.Path(sys.argv[1]).resolve()
    edit(root / "build.sbt", [
        ('ThisBuild / scalaVersion := "3.8.4"', 'ThisBuild / scalaVersion := "3.9.0"'),
        ('"com.softwaremill.ox" %% "core" % "1.0.6"',
         '"com.softwaremill.ox" %% "core" % "1.0.6",\n            "dev.okay" %% "okay-platform" % "0.2.0-SNAPSHOT"'),
    ])
    direct = root / "io-bench/src/main/scala/bench/direct/Benchmarks.scala"
    run = "bench.okay.OkayFiveWay.run(runtime)"
    edit(direct, [
        ('@Param(Array("ce", "kyo", "loom", "ox", "gears")) var runtime',
         f'@Param(Array("ce", "kyo", "loom", "ox", "gears", {OKAY})) var runtime'),
        ('if runtime != "ce" && runtime != "kyo" then backend',
         'if runtime != "ce" && runtime != "kyo" && !bench.okay.OkayFiveWay.isOkay(runtime) then backend'),
        ('case "kyo" => reference.kyoWorkers()\n        case _ =>',
         'case "kyo" => reference.kyoWorkers()\n'
         f'        case "okay" | "okayOwn" => {run}(bench.okay.OkayFiveWay.workers(reference.values, parallelism)(i => okay.async(Work(i, work))))\n'
         '        case _ =>'),
        ('case "kyo" => reference.kyoSpawnJoin()\n        case _ =>',
         'case "kyo" => reference.kyoSpawnJoin()\n'
         f'        case "okay" | "okayOwn" => {run}(bench.okay.OkayFiveWay.spawnJoin(ops))\n'
         '        case _ =>'),
        ('case "kyo" => reference.kyoRunner()\n        case _ =>',
         'case "kyo" => reference.kyoRunner()\n'
         f'        case "okay" | "okayOwn" => {run}(okay.pure[okay.Async, Int](1))\n'
         '        case _ =>'),
    ])
    io = root / "io-bench/src/main/scala/bench/io/IoBench.scala"
    edit(io, [
        ('@Param(Array("ce", "kyo", "loom", "gears", "ox")) var runtime',
         f'@Param(Array("ce", "kyo", "loom", "gears", "ox", {OKAY})) var runtime'),
        ('require(Set("ce", "kyo", "loom", "gears", "ox", "ceVirtual", "kyoFlush", "kyoTuned").contains(runtime))',
         f'require(Set("ce", "kyo", "loom", "gears", "ox", "ceVirtual", "kyoFlush", "kyoTuned", {OKAY}).contains(runtime))'),
        ('require(Set("ce", "kyo", "loom", "gears", "ox")(runtime) || transport == "blocking")',
         f'require(Set("ce", "kyo", "loom", "gears", "ox", {OKAY})(runtime) || transport == "blocking")'),
        ('case "loom" => loomBatch(io)',
         'case "loom" => loomBatch(io)\n'
         f'        case "okay" | "okayOwn" => {run}(bench.okay.OkayFiveWay.batch(io, size, parallelism, transport == "blocking"))'),
    ])
    # the TCP validation counts calls made on virtual threads: okay's
    # default scheduler is one virtual thread per fiber, `own` is not
    edit(root / "io-bench/src/test/scala/bench/io/Validation.scala", [
        ('Set("loom", "gears", "ox", "ceVirtual")(runtime)', 'Set("loom", "gears", "ox", "ceVirtual", "okay")(runtime)'),
    ])
    main_dir = root / "io-bench/src/main/scala/bench/okay"
    test_dir = root / "io-bench/src/test/scala/bench/okay"
    main_dir.mkdir(parents=True, exist_ok=True)
    test_dir.mkdir(parents=True, exist_ok=True)
    shutil.copy(HERE / "OkayFiveWay.scala", main_dir / "OkayFiveWay.scala")
    shutil.copy(HERE / "OkayValidation.scala", test_dir / "OkayValidation.scala")
    print(f"okay added to {root}")


if __name__ == "__main__":
    main()
