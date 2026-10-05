package okay.audit

import java.io.{ByteArrayInputStream, DataInputStream}
import java.nio.file.{Files, Path}
import java.util.jar.JarFile
import scala.jdk.CollectionConverters.*

/** one reference a class file makes to something outside itself: the owner
  * class, the member and its descriptor (empty for a bare class reference);
  * `Native` is the class's OWN method carrying `ACC_NATIVE` — not a reference,
  * but the same kind of reach past the boundary, so it is reported as one */
final case class Ref(from: String, kind: Ref.Kind, owner: String, name: String, descriptor: String):
  /** `java.lang.System#currentTimeMillis` — how a rule and a report name it */
  def member: String = if name.isEmpty then owner else s"$owner#$name"

object Ref:
  enum Kind:
    case Class, Field, Method, InterfaceMethod, Native

/** the class-file reader: JVMS §4, the constant pool and the methods'
  * access flags, nothing else. Not `java.lang.classfile` — dotty 3.9 cannot
  * load its sealed model types (specs/core-gaps.md, stage 4), and the pool
  * is one screen. The reader of `TestInlineBudget` extended from "a
  * method's code length" to "everything the class refers to". */
object Scan:

  def classFile(bytes: Array[Byte]): Vector[Ref] =
    val in = DataInputStream(ByteArrayInputStream(bytes))
    if in.readInt() != 0xCAFEBABE then throw IllegalArgumentException("not a class file")
    in.readUnsignedShort(); in.readUnsignedShort()                      // minor, major
    val count = in.readUnsignedShort()
    val tags = new Array[Int](count)
    val utf8 = new Array[String](count)
    val a = new Array[Int](count)                                       // class_index / name_index
    val b = new Array[Int](count)                                       // name_and_type_index / descriptor_index
    var i = 1
    while i < count do
      val tag = in.readUnsignedByte()
      tags(i) = tag
      tag match
        case 1 => utf8(i) = in.readUTF()
        case 7 | 8 | 16 | 19 | 20 => a(i) = in.readUnsignedShort()     // Class, String, MethodType, Module, Package
        case 9 | 10 | 11 | 12 | 17 | 18 => a(i) = in.readUnsignedShort(); b(i) = in.readUnsignedShort()
        case 3 | 4 => in.skipNBytes(4)
        case 5 | 6 => in.skipNBytes(8); i += 1                          // a long or double takes two slots
        case 15 => in.readUnsignedByte(); a(i) = in.readUnsignedShort() // MethodHandle: kind, reference
        case other => throw IllegalArgumentException(s"constant pool tag $other unknown")
      i += 1
    def className(idx: Int): String = utf8(a(idx)).replace('/', '.')
    in.readUnsignedShort()                                              // access
    val self = className(in.readUnsignedShort())
    in.readUnsignedShort()                                              // super
    in.skipNBytes(2L * in.readUnsignedShort())                          // interfaces
    def skipAttributes(): Unit =
      for _ <- 0 until in.readUnsignedShort() do
        in.readUnsignedShort(); in.skipNBytes(in.readInt() & 0xFFFFFFFFL)
    for _ <- 0 until in.readUnsignedShort() do                          // fields
      in.skipNBytes(6); skipAttributes()
    val natives = Vector.newBuilder[Ref]
    for _ <- 0 until in.readUnsignedShort() do                          // methods
      val access = in.readUnsignedShort()
      val name = utf8(in.readUnsignedShort())
      val desc = utf8(in.readUnsignedShort())
      if (access & 0x0100) != 0 then natives += Ref(self, Ref.Kind.Native, self, name, desc)
      skipAttributes()
    val refs = Vector.newBuilder[Ref]
    // a class the pool names only through a member reference is covered by
    // that reference; a bare CONSTANT_Class (new, checkcast, classOf) is its own
    var k = 1
    while k < count do
      tags(k) match
        case 7 if utf8(a(k)).charAt(0) != '[' =>
          val owner = className(k)
          if owner != self then refs += Ref(self, Ref.Kind.Class, owner, "", "")
        case 9 | 10 | 11 =>
          val owner = className(a(k))
          if owner != self then
            val nt = b(k)
            val kind = tags(k) match
              case 9 => Ref.Kind.Field
              case 10 => Ref.Kind.Method
              case _ => Ref.Kind.InterfaceMethod
            refs += Ref(self, kind, owner, utf8(a(nt)), utf8(b(nt)))
        case _ =>
      k += 1
    (refs.result() ++ natives.result()).distinct

  def directory(dir: Path): Vector[Ref] =
    if !Files.isDirectory(dir) then Vector.empty
    else
      val files = Files.walk(dir).iterator().asScala.filter(p => p.toString.endsWith(".class")).toVector.sortBy(_.toString)
      files.flatMap(p => classFile(Files.readAllBytes(p)))

  def jar(path: Path): Vector[Ref] =
    if !Files.isRegularFile(path) then Vector.empty
    else
      val jf = JarFile(path.toFile)
      try
        jf.entries().asScala.toVector.filter(e => e.getName.endsWith(".class") && !e.getName.startsWith("META-INF/"))
          .sortBy(_.getName)
          .flatMap(e => classFile(jf.getInputStream(e).readAllBytes()))
      finally jf.close()

  def path(p: Path): Vector[Ref] = if Files.isDirectory(p) then directory(p) else jar(p)

  def classpath(entries: Seq[Path]): Map[Path, Vector[Ref]] = entries.map(p => p -> path(p)).toMap

  /** sha-256 of what was scanned — a jar's bytes, or a directory's class files in path order */
  def digest(p: Path): String =
    val md = java.security.MessageDigest.getInstance("SHA-256")
    if Files.isDirectory(p) then
      Files.walk(p).iterator().asScala.filter(f => f.toString.endsWith(".class")).toVector.sortBy(_.toString)
        .foreach { f => md.update(p.relativize(f).toString.getBytes("UTF-8")); md.update(Files.readAllBytes(f)) }
    else if Files.isRegularFile(p) then md.update(Files.readAllBytes(p))
    md.digest().map(x => f"$x%02x").mkString
