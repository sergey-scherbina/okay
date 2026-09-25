package okay

import java.lang.foreign.{Arena, FunctionDescriptor, Linker, MemorySegment, ValueLayout}
import java.lang.invoke.MethodHandle

/**
 * The JDK 22+ variant of `okay.StackRoom` (specs/cont-stack.md Layer
 * 3, Decision 12): the stack pointer and this thread's stack bounds,
 * read through the Foreign Function & Memory API. Compiled as project
 * `okayJdk22` (`-java-output-version 22`, so nothing past 22 is used)
 * and packaged under `META-INF/versions/22/` of okay's jar; a JVM of
 * 22 or newer loads this class in place of the root one (JEP 238).
 *
 * Two gates, both silent:
 * - `Module.isNativeAccessEnabled`: restricted methods (`downcallHandle`)
 *   print four WARNING lines on JDK 24+ when the user did not pass
 *   `--enable-native-access`, and a later release refuses them
 *   (JEP 472). A library does not print that on a user's console, so
 *   without the flag this object answers −1 like the root and never
 *   touches `Linker`.
 * - the `(os, arch)` layout: the pointer comes out of `getcontext`'s
 *   `ucontext_t`, whose `sp` sits at an offset that differs per OS and
 *   architecture. Measured 2026-09-25 on macOS arm64 (probe
 *   `StackProbe`, JDK 26): `uc_mcontext` at 48, `__ss.__sp` at 264
 *   into it — 1000 frames of a trivial compiled method read 112 B
 *   each, agreeing with the frame arithmetic. The other three layouts
 *   are backlog `cont-stack-ucontext-layouts`; an unmeasured pair
 *   answers −1. musl (Alpine) has no `getcontext`: a missing symbol
 *   falls through the same way.
 *
 * From a VIRTUAL thread the bounds are the CARRIER's (measured), which
 * is the stack in use: a mounted virtual thread grows on its carrier.
 *
 * Cost: 326 ns a `sp()` (a `sigprocmask` syscall inside `getcontext`),
 * paid once per grant at exhaustion, never per level.
 */
private[okay] object StackRoom:

  /** a measured layout: where `sp` is inside `ucontext_t` */
  private final class Layout(val ucMcontext: Long, val spInMcontext: Long)

  private val layout: Layout | Null =
    (System.getProperty("os.name", ""), System.getProperty("os.arch", "")) match
      case (os, "aarch64") if os.startsWith("Mac") => Layout(48, 264)
      case _ => null

  private val enabled: Boolean =
    layout != null && classOf[StackRoom.type].getModule.isNativeAccessEnabled

  /** the four handles, or null when any of them could not be made */
  private final class Handles(val self: MethodHandle, val addr: MethodHandle, val size: MethodHandle, val getcontext: MethodHandle, val pagesize: MethodHandle)

  private val handles: Handles | Null =
    if !enabled then null
    else
      try
        val l = Linker.nativeLinker()
        val s = l.defaultLookup()
        def h(name: String, d: FunctionDescriptor): MethodHandle | Null =
          s.find(name).map[MethodHandle | Null](seg => l.downcallHandle(seg, d)).orElse(null)
        val self = h("pthread_self", FunctionDescriptor.of(ValueLayout.ADDRESS))
        val addr = h("pthread_get_stackaddr_np", FunctionDescriptor.of(ValueLayout.ADDRESS, ValueLayout.ADDRESS))
        val size = h("pthread_get_stacksize_np", FunctionDescriptor.of(ValueLayout.JAVA_LONG, ValueLayout.ADDRESS))
        val gc = h("getcontext", FunctionDescriptor.of(ValueLayout.JAVA_INT, ValueLayout.ADDRESS))
        val ps = h("getpagesize", FunctionDescriptor.of(ValueLayout.JAVA_INT))
        if self == null || addr == null || size == null || gc == null || ps == null then null
        else Handles(self, addr, size, gc, ps)
      catch case _: Throwable => null

  private val ucontextBytes = 1024L

  /**
   * THE FLOOR IS NOT THE END OF THE STACK. HotSpot keeps guard zones at
   * the end — red, yellow, reserved — and bangs `StackShadowPages`
   * ahead of every frame it pushes, so a `StackOverflowError` strikes
   * when the pointer comes within (shadow + yellow + red + reserved)
   * pages of the end: 24 pages × 16 KB = 384 KB on macOS arm64
   * (measured 2026-09-25: with the floor at `top − size` the runner
   * granted down to 67 KB above it and overflowed with no switch).
   * The pages are the VM's own flags, the page size is libc's.
   */
  private val zoneBytes: Long =
    val pages =
      try
        val bean = java.lang.management.ManagementFactory.getPlatformMXBean(classOf[com.sun.management.HotSpotDiagnosticMXBean])
        List("StackShadowPages", "StackYellowPages", "StackRedPages", "StackReservedPages")
          .map(n => bean.getVMOption(n).getValue.toLong).sum
      catch case _: Throwable => 24L
    val page =
      val hs = handles
      if hs == null then 16384L
      else
        try
          val p: Int = hs.pagesize.invokeExact()
          if p > 0 then p.toLong else 16384L
        catch case _: Throwable => 16384L
    pages * page

  def sp(): Long =
    val hs = handles
    val lay = layout
    if hs == null || lay == null then -1L
    else
      try
        val arena = Arena.ofConfined()
        try
          val uc = arena.allocate(ucontextBytes, 16)
          val r: Int = hs.getcontext.invokeExact(uc) // signature-polymorphic: the ascription IS the descriptor
          if r != 0 then -1L
          else
            val mc = uc.get(ValueLayout.ADDRESS, lay.ucMcontext).reinterpret(ucontextBytes)
            mc.get(ValueLayout.JAVA_LONG, lay.spInMcontext)
        finally arena.close()
      catch case _: Throwable => -1L

  private def thread(hs: Handles): MemorySegment =
    val t: MemorySegment = hs.self.invokeExact()
    t

  def top(): Long =
    val hs = handles
    if hs == null then -1L
    else
      try
        val a: MemorySegment = hs.addr.invokeExact(thread(hs))
        a.address()
      catch case _: Throwable => -1L

  def floor(): Long =
    val hs = handles
    if hs == null then -1L
    else
      try
        val t = thread(hs)
        val a: MemorySegment = hs.addr.invokeExact(t)
        val size: Long = hs.size.invokeExact(t)
        a.address() - size + zoneBytes
      catch case _: Throwable => -1L
