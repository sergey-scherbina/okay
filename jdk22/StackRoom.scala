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
 *   architecture, and the bounds come from a call that differs per OS.
 *   Every layout below was MEASURED, never taken from a header alone
 *   (specs/cont-stack.md Decision 13): a probe scanned the whole
 *   `ucontext_t` for the words inside the thread's bounds that fall as
 *   the stack deepens, and exactly two did — `sp` and the frame pointer,
 *   moving together.
 *   - macOS arm64 (2026-09-25, JDK 26): `uc_mcontext` is a POINTER at
 *     48, `__ss.__sp` at 264 behind it; bounds from
 *     `pthread_get_stackaddr_np`/`pthread_get_stacksize_np`.
 *   - glibc aarch64 (2026-09-26, Docker linux/arm64 on Apple silicon,
 *     native, JDK 26): `uc_mcontext` INLINE, `sp` at 432 (fault
 *     address, x0..x30, sp); the frame pointer x29 at 416 moved with it.
 *   - glibc x86_64 (2026-09-26, Docker linux/amd64 UNDER EMULATION on
 *     Apple silicon, JDK 26): inline, `gregs[REG_RSP]` at 160
 *     (40 + 15 × 8); RBP at 120 moved with it.
 *   On glibc the bounds are `pthread_getattr_np` +
 *   `pthread_attr_getstack`, the guard (`pthread_attr_getguardsize`)
 *   taken off the bottom — what HotSpot's own
 *   `os::Linux::current_stack_region` does. macOS x86_64 is not
 *   measured and answers −1. musl (Alpine) has no `getcontext`: a
 *   missing symbol falls through the same way (`readableWithout`).
 *
 * From a VIRTUAL thread the bounds are the CARRIER's (measured), which
 * is the stack in use: a mounted virtual thread grows on its carrier.
 *
 * Cost: 326 ns a `sp()` (a `sigprocmask` syscall inside `getcontext`),
 * paid once per grant at exhaustion, never per level.
 */
private[okay] object StackRoom:

  /**
   * a measured layout: where `sp` is inside `ucontext_t` — behind the
   * `uc_mcontext` POINTER at `mcontextPointerAt` (macOS), or inline at
   * `spAt` when `mcontextPointerAt` is −1 (glibc) — how many bytes to
   * hand `getcontext`, and which call gives the bounds
   *
   * THE BUFFER IS THE STRUCT, NOT THE BYTES SEEN WRITTEN: measured,
   * `getcontext` writes up to byte 608 on macOS arm64, 1004 on glibc
   * aarch64 and 452 on glibc x86_64, but glibc aarch64's `ucontext_t`
   * is 4560 bytes (its `__reserved` area holds whatever extension
   * records the kernel and libc add), so a glibc read gets 8192
   */
  private final class Layout(val mcontextPointerAt: Long, val spAt: Long, val ucontextBytes: Long, val glibc: Boolean)

  private val layout: Layout | Null =
    (System.getProperty("os.name", ""), System.getProperty("os.arch", "")) match
      case (os, "aarch64") if os.startsWith("Mac") => Layout(48, 264, 1024, glibc = false)
      case ("Linux", "aarch64") => Layout(-1, 432, 8192, glibc = true)
      case ("Linux", "amd64") => Layout(-1, 160, 8192, glibc = true)
      case _ => null

  private val enabled: Boolean =
    layout != null && classOf[StackRoom.type].getModule.isNativeAccessEnabled

  /**
   * the handles one layout needs, or null when any of them could not be
   * made: `getcontext` and `getpagesize` everywhere, and the bounds —
   * `stackaddr`/`stacksize` (macOS) or `getattr`/`getstack`/
   * `guardsize`/`destroy` (glibc); the other pair is null
   */
  private final class Handles(
      val self: MethodHandle, val getcontext: MethodHandle, val pagesize: MethodHandle,
      val addr: MethodHandle | Null, val size: MethodHandle | Null,
      val getattr: MethodHandle | Null, val getstack: MethodHandle | Null,
      val guardsize: MethodHandle | Null, val destroy: MethodHandle | Null)

  /** the handles for `lay`, looked up with the named symbol HIDDEN — the
   * one door the tests use to make a platform without it (musl has no
   * `getcontext`); production hides nothing */
  private def handlesFor(lay: Layout, hidden: String): Handles | Null =
    try
      val l = Linker.nativeLinker()
      val s = l.defaultLookup()
      def h(name: String, d: FunctionDescriptor): MethodHandle | Null =
        if name == hidden then null
        else s.find(name).map[MethodHandle | Null](seg => l.downcallHandle(seg, d)).orElse(null)
      val A = ValueLayout.ADDRESS
      val I = ValueLayout.JAVA_INT
      val self = h("pthread_self", FunctionDescriptor.of(A))
      val gc = h("getcontext", FunctionDescriptor.of(I, A))
      val ps = h("getpagesize", FunctionDescriptor.of(I))
      if self == null || gc == null || ps == null then null
      else if lay.glibc then
        val getattr = h("pthread_getattr_np", FunctionDescriptor.of(I, A, A))
        val getstack = h("pthread_attr_getstack", FunctionDescriptor.of(I, A, A, A))
        val guard = h("pthread_attr_getguardsize", FunctionDescriptor.of(I, A, A))
        val destroy = h("pthread_attr_destroy", FunctionDescriptor.of(I, A))
        if getattr == null || getstack == null || guard == null || destroy == null then null
        else Handles(self, gc, ps, null, null, getattr, getstack, guard, destroy)
      else
        val addr = h("pthread_get_stackaddr_np", FunctionDescriptor.of(A, A))
        val size = h("pthread_get_stacksize_np", FunctionDescriptor.of(ValueLayout.JAVA_LONG, A))
        if addr == null || size == null then null
        else Handles(self, gc, ps, addr, size, null, null, null, null)
    catch case _: Throwable => null

  private val handles: Handles | Null =
    val lay = layout
    if !enabled || lay == null then null else handlesFor(lay, "")

  /** whether this platform reads with `symbol` absent — false on every
   * JVM that cannot read at all; the tests hide `getcontext` to prove a
   * missing symbol falls through to the count instead of failing */
  def readableWithout(symbol: String): Boolean =
    val lay = layout
    enabled && lay != null && handlesFor(lay, symbol) != null

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
          val uc = arena.allocate(lay.ucontextBytes, 16)
          val r: Int = hs.getcontext.invokeExact(uc) // signature-polymorphic: the ascription IS the descriptor
          if r != 0 then -1L
          else if lay.mcontextPointerAt < 0 then uc.get(ValueLayout.JAVA_LONG, lay.spAt)
          else
            val mc = uc.get(ValueLayout.ADDRESS, lay.mcontextPointerAt).reinterpret(lay.ucontextBytes)
            mc.get(ValueLayout.JAVA_LONG, lay.spAt)
        finally arena.close()
      catch case _: Throwable => -1L

  private def thread(hs: Handles): MemorySegment =
    val t: MemorySegment = hs.self.invokeExact()
    t

  /**
   * this thread's stack as (lowest usable address, highest address),
   * written into `out`; false when unreadable. glibc: the attributes of
   * the running thread, the guard taken off the bottom, as HotSpot's
   * `os::Linux::current_stack_region` does. macOS: the top and the size.
   */
  private def bounds(hs: Handles, out: Array[Long]): Boolean =
    val t = thread(hs)
    val getattr = hs.getattr
    val getstack = hs.getstack
    val guardsize = hs.guardsize
    val destroy = hs.destroy
    val addr = hs.addr
    val size = hs.size
    if getattr != null && getstack != null && guardsize != null && destroy != null then
      val arena = Arena.ofConfined()
      try
        val attr = arena.allocate(256, 16) // pthread_attr_t: 56 B on x86_64, 64 B on aarch64
        val lo = arena.allocate(ValueLayout.JAVA_LONG)
        val sz = arena.allocate(ValueLayout.JAVA_LONG)
        val gd = arena.allocate(ValueLayout.JAVA_LONG)
        val r0: Int = getattr.invokeExact(t, attr)
        if r0 != 0 then false
        else
          try
            val r1: Int = getstack.invokeExact(attr, lo, sz)
            val r2: Int = guardsize.invokeExact(attr, gd)
            if r1 != 0 || r2 != 0 then false
            else
              val low = lo.get(ValueLayout.JAVA_LONG, 0)
              out(0) = low + gd.get(ValueLayout.JAVA_LONG, 0)
              out(1) = low + sz.get(ValueLayout.JAVA_LONG, 0)
              true
          finally
            val _: Int = destroy.invokeExact(attr)
      finally arena.close()
    else if addr != null && size != null then
      val a: MemorySegment = addr.invokeExact(t)
      val s: Long = size.invokeExact(t)
      out(0) = a.address() - s
      out(1) = a.address()
      true
    else false

  def top(): Long =
    val hs = handles
    if hs == null then -1L
    else
      try
        val b = new Array[Long](2)
        if bounds(hs, b) then b(1) else -1L
      catch case _: Throwable => -1L

  def floor(): Long =
    val hs = handles
    if hs == null then -1L
    else
      try
        val b = new Array[Long](2)
        if bounds(hs, b) then b(0) + zoneBytes else -1L
      catch case _: Throwable => -1L
