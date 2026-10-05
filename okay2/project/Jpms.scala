import sbt._
import sbt.Keys._
import java.lang.module.ModuleFinder
import java.nio.file.Files
import java.util.jar.{JarFile, JarOutputStream, JarEntry, Manifest, Attributes}
import scala.collection.JavaConverters._
import scala.sys.process._

/** Descriptors are compiled after Scala, against temporary automatic views
  * of project products. No dependency's packageBin task is needed here. */
object Jpms {
  val jpmsModuleName = settingKey[String]("Stable okay2 JVM module name")
  val descriptor = taskKey[File]("Compile the JVM module descriptor")
  val externalJars = taskKey[Seq[File]]("Optional interop jars for a module-path bundle")
  val jpmsCheck = taskKey[File]("Resolve real JVM jars and exercise their named service boundary")
  val settings: Seq[Setting[_]] = Seq(
    jpmsModuleName := named(name.value),
    externalJars := {
      val _ = (Compile / compile).value
      val cp = (Compile / dependencyClasspath).value.map(_.data)
      neededExternal((Compile / classDirectory).value, cp, streams.value.log)
    },
    descriptor := {
      val _ = (Compile / compile).value
      val own = (Compile / classDirectory).value
      val cp = (Compile / dependencyClasspath).value.map(_.data)
      compileDescriptor(jpmsModuleName.value, own, cp, externalJars.value,
        (ThisBuild / baseDirectory).value, crossTarget.value / "jpms", streams.value.log)
    },
    Compile / packageOptions += Package.ManifestAttributes("Automatic-Module-Name" -> jpmsModuleName.value),
    Compile / packageBin / mappings += descriptor.value -> "module-info.class",
  )

  def named(artifact: String): String = if (artifact == "okay2") "okay2.core" else artifact.replace('-', '.')
  private def scalaJar(file: File): Boolean = file.getName.startsWith("scala-library-") || file.getName.startsWith("scala-reflect-")

  private def projectModule(file: File, root: File): String = {
    val relative = IO.relativize(root, file).getOrElse(sys.error("JPMS: product outside okay2: " + file))
    val first = relative.replace('\\', '/').takeWhile(_ != '/')
    if (first == ".jvm") "okay2.core"
    else if (first.startsWith("okay2-")) named(first)
    else sys.error("JPMS: unknown project product: " + file)
  }

  private def packages(dir: File): Vector[String] =
    (dir ** "*.class").get.flatMap { f =>
      val relative = IO.relativize(dir, f).get.replace('\\', '/')
      val slash = relative.lastIndexOf('/')
      if (slash <= 0 || relative.endsWith("module-info.class")) None
      else Some(relative.substring(0, slash).replace('/', '.'))
    }.distinct.sorted.toVector

  private def archive(out: File, module: String, entries: Vector[(String, Array[Byte])]): File = {
    val manifest = new Manifest
    manifest.getMainAttributes.put(Attributes.Name.MANIFEST_VERSION, "1.0")
    manifest.getMainAttributes.putValue("Automatic-Module-Name", module)
    if (entries.exists(_._1.startsWith("META-INF/versions/")))
      manifest.getMainAttributes.putValue("Multi-Release", "true")
    val stream = new JarOutputStream(Files.newOutputStream(out.toPath), manifest)
    try entries.sortBy(_._1).foreach { case (name, bytes) =>
      val entry = new JarEntry(name)
      entry.setTime(0L)
      stream.putNextEntry(entry)
      stream.write(bytes)
      stream.closeEntry()
    } finally stream.close()
    out
  }

  private def automatic(dir: File, module: String, work: File): File =
    archive(work / (module + ".jar"), module, (dir ** "*.class").get
      .filterNot(_.getName == "module-info.class").map(f => IO.relativize(dir, f).get.replace('\\', '/') -> Files.readAllBytes(f.toPath)).toVector)

  /** A selected interop profile's optional jars share one module. Identical
    * classes deduplicate; different definitions fail instead of choosing one. */
  def bundle(jars: Seq[File], out: File): Option[File] = {
    if (jars.isEmpty) None
    else {
      IO.createDirectory(out.getParentFile)
      val entries = scala.collection.mutable.Map.empty[String, (File, Array[Byte])]
      val services = scala.collection.mutable.Map.empty[String, Vector[String]]
      jars.distinct.sortBy(_.toString).foreach { path =>
        val jar = new JarFile(path)
        try jar.entries.asScala.filterNot(_.isDirectory).foreach { e =>
          val n = e.getName
          val discard = n == "META-INF/MANIFEST.MF" || n.endsWith("module-info.class") ||
            (n.startsWith("META-INF/") && (n.endsWith(".SF") || n.endsWith(".RSA") || n.endsWith(".DSA")))
          if (!discard) {
            val in = jar.getInputStream(e)
            val bytes = try in.readAllBytes() finally in.close()
            if (n.startsWith("META-INF/services/")) {
              val lines = new String(bytes, java.nio.charset.StandardCharsets.UTF_8).split("\n").map(_.takeWhile(_ != '#').trim).filter(_.nonEmpty).toVector
              services.update(n, services.getOrElse(n, Vector.empty) ++ lines)
            } else entries.get(n) match {
              case Some((previous, old)) if n.endsWith(".class") && !java.util.Arrays.equals(old, bytes) =>
                sys.error("JPMS: conflicting optional class " + n + " in " + previous + " and " + path)
              case Some(_) =>
              case None => entries.update(n, path -> bytes)
            }
          }
        } finally jar.close()
      }
      val ordinary = entries.iterator.map { case (n, (_, bytes)) => n -> bytes }.toVector
      val serviceEntries = services.iterator.map { case (n, lines) => n -> (lines.distinct.sorted.mkString("\n") + "\n").getBytes(java.nio.charset.StandardCharsets.UTF_8) }.toVector
      Some(archive(out, "okay2.externals", ordinary ++ serviceEntries))
    }
  }

  private def run(args: Seq[String], log: Logger): Vector[String] = {
    val lines = Vector.newBuilder[String]
    val status = Process(args).!(ProcessLogger(s => lines += s, s => { lines += s; log.error(s) }))
    val output = lines.result()
    if (status != 0) sys.error("JPMS: failed " + args.head + "\n" + output.mkString("\n"))
    output
  }

  private def tool(name: String): String = file(sys.props("java.home")) / "bin" / name match { case f => f.toString }

  private def references(own: File, cp: Seq[File], log: Logger): Vector[String] =
    run(Seq(tool("jdeps"), "--multi-release", "17", "-verbose:class", "--class-path",
      cp.mkString(java.io.File.pathSeparator), own.toString), log).flatMap { line =>
      val parts = line.trim.split("\\s+")
      if (parts.length >= 4 && parts(1) == "->") Some(parts.drop(3).mkString(" ")) else None
    }.distinct

  private def neededExternal(own: File, cp: Seq[File], log: Logger): Seq[File] = {
    val available = cp.filter(f => f.isFile && f.getName.endsWith(".jar") && !scalaJar(f)).map(f => f.getName -> f).toMap
    var pending = references(own, cp, log).flatMap(available.get)
    var seen = Set.empty[File]
    while (pending.nonEmpty) {
      val next = pending.head
      pending = pending.tail
      if (!seen(next)) {
        seen += next
        pending ++= references(next, cp, log).flatMap(available.get).filterNot(seen)
      }
    }
    seen.toVector.sortBy(_.toString)
  }

  def verify(jars: Seq[File], cp: Seq[File], root: File, work: File, log: Logger): File = {
    IO.delete(work)
    val lib = work / "lib"
    IO.createDirectory(lib)
    val libraries = cp.filter(f => f.isFile && scalaJar(f)).distinct
    val external = cp.filter(f => f.isFile && f.getName.endsWith(".jar") && !scalaJar(f)).distinct
    val optional = bundle(external, lib / "okay2.externals.jar")
    val fixture = work / "fixtures"
    IO.createDirectory(fixture)
    val a = archive(fixture / "a.jar", "fixture.a", Vector("fixture/Conflict.class" -> Array[Byte](1)))
    val b = archive(fixture / "b.jar", "fixture.b", Vector("fixture/Conflict.class" -> Array[Byte](2)))
    val conflict = try { bundle(Seq(a, b), fixture / "bad.jar"); None } catch {
      case e: RuntimeException => Some(e.getMessage)
    }
    if (!conflict.exists(s => s.contains("fixture/Conflict.class") && s.contains("a.jar") && s.contains("b.jar")))
      sys.error("JPMS: conflicting optional class was not refused by name: " + conflict)
    val c = archive(fixture / "c.jar", "fixture.c", Vector("fixture/Conflict.class" -> Array[Byte](1)))
    val identical = bundle(Seq(a, c), fixture / "identical.jar")
    if (identical.isEmpty) sys.error("JPMS: identical optional classes did not deduplicate")
    val selected = (jars ++ libraries).map { source =>
      val destination = lib / source.getName
      IO.copyFile(source, destination)
      destination
    } ++ optional
    val descriptors = ModuleFinder.of(selected.map(_.toPath): _*).findAll().asScala.toVector.map(_.descriptor())
    val collisions = descriptors.flatMap(d => d.packages().asScala.map(_ -> d.name())).groupBy(_._1)
      .collect { case (p, owners) if owners.map(_._2).distinct.size > 1 => p + ": " + owners.map(_._2).distinct.sorted.mkString(", ") }
    if (collisions.nonEmpty) sys.error("JPMS: split packages\n" + collisions.toVector.sorted.mkString("\n"))
    descriptors.filter(d => d.name().startsWith("okay2.") && d.name() != "okay2.externals").foreach { d =>
      if (d.isAutomatic()) sys.error("JPMS: own artifact is automatic: " + d.name())
    }
    val path = selected.mkString(java.io.File.pathSeparator)
    val classes = work / "probe"
    IO.createDirectory(classes)
    val _ = run(Seq(tool("javac"), "-Xlint:all", "-Werror", "--release", "17", "--module-path", path,
      "-d", classes.toString, (root / "tools/jpms/module-info.java").toString,
      (root / "tools/jpms/BoundaryProbe.java").toString), log)
    run(Seq(tool("java"), "--illegal-native-access=deny", "--module-path", path + java.io.File.pathSeparator + classes,
      "--add-modules", "ALL-MODULE-PATH", "-m", "okay2.probe/okay2.probe.BoundaryProbe", "interop"), log).foreach(log.info(_))
    run(Seq(tool("java"), "--class-path", path + java.io.File.pathSeparator + classes,
      "okay2.probe.BoundaryProbe", "classpath"), log).foreach(log.info(_))
    optional.foreach { omitted =>
      val missingPath = selected.filterNot(_ == omitted).mkString(java.io.File.pathSeparator)
      val output = Vector.newBuilder[String]
      val status = Process(Seq(tool("java"), "--module-path", missingPath + java.io.File.pathSeparator + classes,
        "--add-modules", "ALL-MODULE-PATH", "-m", "okay2.probe/okay2.probe.BoundaryProbe"))
        .!(ProcessLogger(s => output += s, s => output += s))
      if (status == 0 || !output.result().exists(_.contains("okay2.externals")))
        sys.error("JPMS: missing optional interop bundle was not refused by module name")
    }
    // Interop is optional at module selection, not after selecting an adapter.
    val interop = Set("okay2.cats", "okay2.fs2", "okay2.zio", "okay2.externals")
    val corePath = selected.filterNot(f => ModuleFinder.of(f.toPath).findAll().asScala.exists(r => interop(r.descriptor().name())))
      .mkString(java.io.File.pathSeparator)
    run(Seq(tool("java"), "--illegal-native-access=deny", "--module-path", corePath + java.io.File.pathSeparator + classes,
      "--add-modules", "ALL-MODULE-PATH", "-m", "okay2.probe/okay2.probe.BoundaryProbe"), log).foreach(log.info(_))
    log.info("JPMS: " + descriptors.size + " disjoint modules; optional interop profile and dependency-free profile PASS")
    lib
  }

  private def compileDescriptor(module: String, own: File, cp: Seq[File], external: Seq[File], root: File, work: File, log: Logger): File = {
    IO.createDirectory(work)
    val internal = cp.filter(_.isDirectory).map(f => projectModule(f, root) -> f).groupBy(_._1).toVector.map {
      case (n, Seq((_, dir))) => n -> automatic(dir, n, work)
      case (n, _) => sys.error("JPMS: multiple products for " + n)
    }.sortBy(_._1)
    val libraries = cp.filter(f => f.isFile && scalaJar(f))
    val optional = bundle(external, work / "okay2.externals.jar")
    val jdk = references(own, cp, log).flatMap { origin =>
      "(?:java|jdk)\\.[\\w.]+".r.findFirstIn(origin)
    }.distinct.sorted
    val requires = (internal.map { case (n, _) => "    requires transitive " + n + ";" } ++
      Vector("    requires transitive scala.library;") ++
      (if (libraries.exists(_.getName.startsWith("scala-reflect-"))) Vector("    requires static scala.reflect;") else Vector.empty) ++
      optional.toVector.map(_ => "    requires transitive okay2.externals;") ++
      jdk.filterNot(_ == "java.base").map(n => "    requires " + n + ";")).distinct
    val exports = packages(own).map(p => "    exports " + p + ";")
    val service = module match {
      case "okay2.async" => Vector("    uses okay2.async.BlockingDefaults;")
      case "okay2.platform" => Vector("    provides okay2.async.BlockingDefaults with okay2.platform.JvmBlockingDefaults;")
      case _ => Vector.empty
    }
    // Scala's standard jars intentionally remain automatic; the optional
    // interop bundle does too. The module lint also discourages terminal
    // digits, but okay2 is the deliberate project identity, not a version.
    val source = work / "module-info.java"
    IO.write(source, "@SuppressWarnings({\"requires-automatic\", \"requires-transitive-automatic\", \"module\"})\nmodule " + module + " {\n" + (requires ++ exports ++ service).mkString("\n") + "\n}\n")
    val path = (internal.map(_._2) ++ libraries ++ optional).mkString(java.io.File.pathSeparator)
    val _ = run(Seq(tool("javac"), "-Xlint:all", "-Werror", "--release", "17", "--module-path", path,
      "--patch-module", module + "=" + own, "-d", work.toString, source.toString), log)
    work / "module-info.class"
  }
}
