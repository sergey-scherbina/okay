package okay.kernel

import java.nio.file.{Files, Path}
import java.util.jar.{JarEntry, JarOutputStream}

/** a plugin ServiceLoader can construct: a class, a public no-arg constructor */
class HelloPlugin extends Plugin:
  def id = "hello"
  def version = Version("1.0")
  def needs = Vector.empty
  def provides = Vector(Provision.value(TestDiscover.greeting, "1.0")(_ => "hello"))

/** one that is found and cannot be constructed */
class BrokenPlugin extends Plugin:
  throw IllegalStateException("no configuration")
  def id = "broken"
  def version = Version("1.0")
  def needs = Vector.empty
  def provides = Vector.empty

object TestDiscover:
  val greeting: Port[String] = Port.many("greeting", Version("1.0"))

class TestDiscover extends munit.FunSuite:

  test("services loads every provider; one that fails is a LoadFailed and the rest still load") {
    val (found, failed) = Discover.services(getClass.getClassLoader)
    assertEquals(found.map(_.id), Vector("hello"))
    assertEquals(failed.map(_.what), Vector("okay.kernel.BrokenPlugin"))
    assert(failed.head.why.contains("no configuration"), failed.head.why)
  }

  test("a service entry naming a class that is not there is a LoadFailed, not a crash") {
    val dir = Files.createTempDirectory("okay-kernel-plugins")
    jar(dir.resolve("b.jar"), "okay.kernel.NoSuchPlugin")
    jar(dir.resolve("a.jar"), "okay.kernel.HelloPlugin")
    Files.writeString(dir.resolve("notes.txt"), "not a jar")
    // under the platform loader neither class is visible (the jars
    // carry only service files), so both entries fail by name — and
    // neither stops the other
    val (found, failed) = Discover.jars(dir, parent = ClassLoader.getPlatformClassLoader)
    assertEquals(found, Vector.empty)
    assertEquals(failed.size, 2, failed)
  }

  test("jars finds the directory's plugins through the host's classes") {
    val dir = Files.createTempDirectory("okay-kernel-plugins")
    jar(dir.resolve("hello.jar"), "okay.kernel.HelloPlugin")
    val isolated = new ClassLoader(getClass.getClassLoader) {
      // hide this module's own META-INF/services so only the jar speaks
      override def getResources(name: String): java.util.Enumeration[java.net.URL] =
        if name.startsWith("META-INF/services/") then java.util.Collections.emptyEnumeration()
        else super.getResources(name)
    }
    val (found, failed) = Discover.jars(dir, parent = isolated)
    assertEquals(found.map(_.id), Vector("hello"))
    assertEquals(failed, Vector.empty)
    val (r, close) = okay.Resource.open(Kernel.assemble(found))
    assertEquals(r.all(TestDiscover.greeting), Vector("hello"))
    close()
  }

  test("a missing or empty directory is no plugins, not a failure") {
    assertEquals(Discover.jars(Path.of("/no/such/dir")), (Vector.empty, Vector.empty))
    assertEquals(Discover.jars(Files.createTempDirectory("okay-kernel-empty")), (Vector.empty, Vector.empty))
  }

  private def jar(at: Path, provider: String): Unit =
    val out = JarOutputStream(Files.newOutputStream(at))
    try
      out.putNextEntry(JarEntry("META-INF/services/okay.kernel.Plugin"))
      out.write((provider + "\n").getBytes("UTF-8"))
      out.closeEntry()
    finally out.close()
