package okay.audit

import java.lang.management.ManagementFactory
import java.lang.module.ModuleDescriptor
import java.nio.file.{Files, Path}
import java.util.jar.JarFile
import scala.jdk.CollectionConverters.*

enum Enforcement:
  case JvmEnforced, ScanOnly
  def text: String = if this == JvmEnforced then "jvm-enforced" else "scan-only"

final case class ModuleEvidence(module: String, path: Path, name: Option[String], requires: Vector[String], enforcement: Vector[(String, Enforcement)])
final case class SplitPackage(name: String, inputs: Vector[String])
final case class JpmsEvidence(modules: Vector[ModuleEvidence], splits: Vector[SplitPackage], launcherOptions: Vector[String])
final case class RuntimeModule(name: String, requires: Vector[String])
final case class RuntimeEvidence(modules: Vector[RuntimeModule], inputArguments: Vector[String], nativeAccess: Vector[String])

object Jpms:
  private val ruleModules = Map("java.sql." -> "java.sql", "javax.sql." -> "java.sql", "javax.naming." -> "java.naming", "java.rmi." -> "java.rmi", "java.net.http." -> "java.net.http")
  private val launcherPrefixes = Vector("--illegal-native-access", "--add-opens", "--add-exports", "--add-reads", "--enable-native-access")

  def launcherOptions(file: Path): Vector[String] =
    if !Files.isRegularFile(file) then throw Audit.Refused(s"jvm options is not a file: $file")
    Files.readAllLines(file).asScala.map(_.trim).filter(s => s.nonEmpty && !s.startsWith("#")).flatMap { option =>
      launcherPrefixes.find(p => option == p || option.startsWith(p + "=")) match
        case Some(prefix) =>
          if option == prefix || option.substring(prefix.length + 1).trim.isEmpty then throw Audit.Refused(s"jvm options at $file: $prefix needs a value")
          Some(option)
        case None => None
    }.toVector

  def runtime(): RuntimeEvidence =
    val modules = ModuleLayer.boot.modules.asScala.toVector.map(m => RuntimeModule(m.getName, m.getDescriptor.requires.asScala.map(_.name).toVector.sorted)).sortBy(_.name)
    val args = ManagementFactory.getRuntimeMXBean.getInputArguments.asScala.toVector.sorted
    RuntimeEvidence(modules, args, args.filter(_.startsWith("--enable-native-access")))

  def evidence(inputs: Vector[(String, Path)], launcher: Vector[String] = Vector.empty): JpmsEvidence =
    val modules = inputs.map { (logical, path) =>
      val desc = descriptor(path)
      val requires = desc.map(_.requires.asScala.map(_.name).toSet).getOrElse(Set.empty)
      val states = Boundary.Default.map { rule => rule.api -> (ruleModules.get(rule.api) match
        // An unresolved dependency may grant readability through requires
        // transitive. Only a java.base-only descriptor proves its absence here.
        case Some(target) if !requires(target) && desc.nonEmpty && requires.subsetOf(Set("java.base")) && !launcher.exists(_.startsWith("--add-reads=")) => Enforcement.JvmEnforced
        case _ => Enforcement.ScanOnly) }.sortBy(_._1)
      ModuleEvidence(logical, path, desc.map(_.name), requires.toVector.sorted, states)
    }.sortBy(m => (m.module, m.path.toString))
    val splits = inputs.groupBy(_._2.toAbsolutePath.normalize).toVector.flatMap { (path, owners) => packages(path).map(_ -> s"${owners.map(_._1).distinct.sorted.mkString("/")}:$path") }.groupBy(_._1).toVector.collect {
      case (name, xs) if xs.map(_._2).distinct.size > 1 => SplitPackage(name, xs.map(_._2).distinct.sorted.toVector)
    }.sortBy(_.name)
    JpmsEvidence(modules, splits, launcher.sorted)

  private def descriptor(path: Path): Option[ModuleDescriptor] =
    try if Files.isDirectory(path) then
      val info = path.resolve("module-info.class")
      if Files.isRegularFile(info) then
        val stream = Files.newInputStream(info)
        try Some(ModuleDescriptor.read(stream)) finally stream.close()
      else None
    else if Files.isRegularFile(path) then
      val jar = JarFile(path.toFile)
      try Option(jar.getJarEntry("module-info.class")).map { e =>
        val stream = jar.getInputStream(e)
        try ModuleDescriptor.read(stream) finally stream.close()
      }
      finally jar.close()
    else None
    catch case e: Exception => throw Audit.Refused(s"JPMS descriptor at $path is invalid: ${e.getMessage}")

  private def packages(path: Path): Set[String] =
    def packageOf(name: String): Option[String] =
      val slash = name.lastIndexOf('/'); if slash <= 0 || name.endsWith("module-info.class") then None else Some(name.substring(0, slash).replace('/', '.'))
    if Files.isDirectory(path) then
      val stream = Files.walk(path)
      try stream.iterator.asScala.filter(p => p.toString.endsWith(".class")).flatMap(p => packageOf(path.relativize(p).toString.replace('\\', '/'))).toSet
      finally stream.close()
    else if Files.isRegularFile(path) then
      val jar = JarFile(path.toFile)
      try jar.entries.asScala.filter(e => e.getName.endsWith(".class") && !e.getName.startsWith("META-INF/")).flatMap(e => packageOf(e.getName)).toSet
      finally jar.close()
    else Set.empty
