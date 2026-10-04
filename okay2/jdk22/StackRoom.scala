package okay2

import java.lang.foreign.{Arena, FunctionDescriptor, Linker, MemorySegment, ValueLayout}
import java.lang.invoke.MethodHandle

/**
 * The JDK 22+ stack reader (okay2-stackroom-jdk22, the Scala 3 core's `jdk22/StackRoom.scala`, specs/cont-stack.md
 * Layer 3): the running thread's stack pointer and its bounds, read through FFM, so `StackSwitch.more` can grant the
 * levels the stack really has before it switches. A Multi-Release class: the root-path `StackRoom` (every method
 * −1) is what a JVM below 22 loads; this one, under `META-INF/versions/22/`, what 22+ loads. Readable only where the
 * `ucontext` layout is known (macOS aarch64, glibc Linux aarch64/x86_64) and native access is enabled; everywhere
 * else every method answers −1, and okay2 counts as before.
 */
private[okay2] object StackRoom {

  /** where `getcontext`'s `ucontext_t` keeps the stack pointer, per platform (the Scala 3 core's measured layouts) */
  private final class Layout(val mcontextPointerAt: Long, val spAt: Long, val ucontextBytes: Long, val glibc: Boolean)

  private val layout: Layout =
    (System.getProperty("os.name", ""), System.getProperty("os.arch", "")) match {
      case (os, "aarch64") if os.startsWith("Mac") => new Layout(48, 264, 1024, glibc = false)
      case ("Linux", "aarch64") => new Layout(-1, 432, 8192, glibc = true)
      case ("Linux", "amd64") => new Layout(-1, 160, 8192, glibc = true)
      case _ => null
    }

  private val enabled: Boolean = layout != null && StackRoom.getClass.getModule.isNativeAccessEnabled

  /** the native calls, null where a symbol is missing (musl has no `pthread_getattr_np`) */
  private final class Handles(
      val self: MethodHandle, val getcontext: MethodHandle, val pagesize: MethodHandle,
      val addr: MethodHandle, val size: MethodHandle,
      val getattr: MethodHandle, val getstack: MethodHandle,
      val guardsize: MethodHandle, val destroy: MethodHandle)

  /** `hidden`: a symbol taken as missing, for the test that the reader declines without it */
  private def handlesFor(lay: Layout, hidden: String): Handles =
    try {
      val l = Linker.nativeLinker()
      val s = l.defaultLookup()
      def h(name: String, d: FunctionDescriptor): MethodHandle =
        if (name == hidden) null
        else {
          val found = s.find(name)
          if (found.isPresent) l.downcallHandle(found.get, d) else null
        }
      val A = ValueLayout.ADDRESS
      val I = ValueLayout.JAVA_INT
      val self = h("pthread_self", FunctionDescriptor.of(A))
      val gc = h("getcontext", FunctionDescriptor.of(I, A))
      val ps = h("getpagesize", FunctionDescriptor.of(I))
      if (self == null || gc == null || ps == null) null
      else if (lay.glibc) {
        val getattr = h("pthread_getattr_np", FunctionDescriptor.of(I, A, A))
        val getstack = h("pthread_attr_getstack", FunctionDescriptor.of(I, A, A, A))
        val guard = h("pthread_attr_getguardsize", FunctionDescriptor.of(I, A, A))
        val destroy = h("pthread_attr_destroy", FunctionDescriptor.of(I, A))
        if (getattr == null || getstack == null || guard == null || destroy == null) null
        else new Handles(self, gc, ps, null, null, getattr, getstack, guard, destroy)
      } else {
        val addr = h("pthread_get_stackaddr_np", FunctionDescriptor.of(A, A))
        val size = h("pthread_get_stacksize_np", FunctionDescriptor.of(ValueLayout.JAVA_LONG, A))
        if (addr == null || size == null) null
        else new Handles(self, gc, ps, addr, size, null, null, null, null)
      }
    } catch { case _: Throwable => null }

  private val handles: Handles = if (!enabled || layout == null) null else handlesFor(layout, "")

  def readable: Boolean = handles != null

  def readableWithout(symbol: String): Boolean = enabled && layout != null && handlesFor(layout, symbol) != null

  /** the guard zones HotSpot keeps at the stack's end, in bytes: a floor that leaves them alone */
  private val zoneBytes: Long = {
    val pages =
      try {
        val bean = java.lang.management.ManagementFactory.getPlatformMXBean(classOf[com.sun.management.HotSpotDiagnosticMXBean])
        List("StackShadowPages", "StackYellowPages", "StackRedPages", "StackReservedPages")
          .map(n => bean.getVMOption(n).getValue.toLong).sum
      } catch { case _: Throwable => 24L }
    val page =
      if (handles == null) 16384L
      else
        try {
          val p = (handles.pagesize.invokeExact(): Int)
          if (p > 0) p.toLong else 16384L
        } catch { case _: Throwable => 16384L }
    pages * page
  }

  /** the stack pointer now, −1 when unreadable */
  def sp(): Long = {
    val hs = handles
    val lay = layout
    if (hs == null || lay == null) -1L
    else
      try {
        val arena = Arena.ofConfined()
        try {
          val uc = arena.allocate(lay.ucontextBytes, 16)
          // signature-polymorphic: the ascription IS the descriptor
          val r = (hs.getcontext.invokeExact(uc): Int)
          if (r != 0) -1L
          else if (lay.mcontextPointerAt < 0) uc.get(ValueLayout.JAVA_LONG, lay.spAt)
          else {
            val mc = uc.get(ValueLayout.ADDRESS, lay.mcontextPointerAt).reinterpret(lay.ucontextBytes)
            mc.get(ValueLayout.JAVA_LONG, lay.spAt)
          }
        } finally arena.close()
      } catch { case _: Throwable => -1L }
  }

  private def thread(hs: Handles): MemorySegment = (hs.self.invokeExact(): MemorySegment)

  /** the stack's usable low end (above the guard) and its top, into `out`; false when they cannot be read */
  private def bounds(hs: Handles, out: Array[Long]): Boolean = {
    val t = thread(hs)
    if (hs.getattr != null && hs.getstack != null && hs.guardsize != null && hs.destroy != null) {
      val arena = Arena.ofConfined()
      try {
        val attr = arena.allocate(256, 16) // pthread_attr_t: 56 B on x86_64, 64 B on aarch64
        val lo = arena.allocate(ValueLayout.JAVA_LONG)
        val sz = arena.allocate(ValueLayout.JAVA_LONG)
        val gd = arena.allocate(ValueLayout.JAVA_LONG)
        val r0 = (hs.getattr.invokeExact(t, attr): Int)
        if (r0 != 0) false
        else
          try {
            val r1 = (hs.getstack.invokeExact(attr, lo, sz): Int)
            val r2 = (hs.guardsize.invokeExact(attr, gd): Int)
            if (r1 != 0 || r2 != 0) false
            else {
              val low = lo.get(ValueLayout.JAVA_LONG, 0)
              out(0) = low + gd.get(ValueLayout.JAVA_LONG, 0)
              out(1) = low + sz.get(ValueLayout.JAVA_LONG, 0)
              true
            }
          } finally {
            val _ = (hs.destroy.invokeExact(attr): Int)
          }
      } finally arena.close()
    } else if (hs.addr != null && hs.size != null) {
      val a = (hs.addr.invokeExact(t): MemorySegment)
      val s = (hs.size.invokeExact(t): Long)
      out(0) = a.address() - s
      out(1) = a.address()
      true
    } else false
  }

  /** the stack's top (its highest address), −1 when unreadable */
  def top(): Long = {
    val hs = handles
    if (hs == null) -1L
    else
      try {
        val b = new Array[Long](2)
        if (bounds(hs, b)) b(1) else -1L
      } catch { case _: Throwable => -1L }
  }

  /** the lowest address a frame may reach, the guard zones left alone; −1 when unreadable */
  def floor(): Long = {
    val hs = handles
    if (hs == null) -1L
    else
      try {
        val b = new Array[Long](2)
        if (bounds(hs, b)) b(0) + zoneBytes else -1L
      } catch { case _: Throwable => -1L }
  }
}
