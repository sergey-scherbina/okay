/*
 * Where does `getcontext` put the stack pointer on THIS platform?
 * (cont-stack-ucontext-layouts, 2026-09-26; specs/cont-stack.md Decision 13)
 *
 * jdk22/StackRoom.scala reads `sp` out of a `ucontext_t` at a per-(os, arch)
 * offset, and an offset is opened only once it has been MEASURED on that OS.
 * This probe measures it without trusting a header: it fills a 16 KB buffer,
 * calls `getcontext` at depth 0 and again 1000 non-inlined frames deeper, and
 * prints every 8-byte word that lies inside the thread's stack bounds both
 * times and FELL between them. On a glibc layout exactly two words do: the
 * stack pointer and the frame pointer, falling by the same amount.
 *
 * Bounds here are glibc's (`pthread_getattr_np`); on macOS use
 * `pthread_get_stackaddr_np`/`pthread_get_stacksize_np` instead.
 *
 * Run (JDK 22+), e.g. in a container for another OS/arch:
 *   java --enable-native-access=ALL-UNNAMED \
 *        -XX:CompileCommand=quiet -XX:CompileCommand=dontinline,UcProbe::deep \
 *        scripts/ucontext-probe.java
 *
 * Measured 2026-09-26, JDK 26:
 *   glibc aarch64 (Docker linux/arm64, native):    sp at 432, x29 at 416, 32 032 B over 1000 frames
 *   glibc x86_64  (Docker linux/amd64, EMULATED):  sp at 160, rbp at 120, 32 048 B over 1000 frames
 */
import java.lang.foreign.*;
import java.lang.invoke.MethodHandle;
import java.util.*;

/** Finds where getcontext stores sp: words inside the thread's stack bounds that move with depth. */
public class UcProbe {
  static final Linker L = Linker.nativeLinker();
  static final SymbolLookup S = L.defaultLookup();
  static MethodHandle h(String n, FunctionDescriptor d) { return L.downcallHandle(S.find(n).orElseThrow(() -> new RuntimeException("missing " + n)), d); }
  static final int UC = 16384;
  static MethodHandle GC;
  static long[] snap() throws Throwable {
    try (Arena a = Arena.ofConfined()) {
      MemorySegment uc = a.allocate(UC, 16);
      int r = (int) GC.invokeExact(uc);
      if (r != 0) throw new RuntimeException("getcontext " + r);
      long[] w = new long[UC / 8];
      for (int i = 0; i < w.length; i++) w[i] = uc.get(ValueLayout.JAVA_LONG, i * 8L);
      return w;
    }
  }
  static long[] deep(int n) throws Throwable { return n == 0 ? snap() : id(deep(n - 1)); }
  static long[] id(long[] x) { return x; }
  public static void main(String[] args) throws Throwable {
    System.out.println("os=" + System.getProperty("os.name") + " arch=" + System.getProperty("os.arch") + " jdk=" + Runtime.version());
    GC = h("getcontext", FunctionDescriptor.of(ValueLayout.JAVA_INT, ValueLayout.ADDRESS));
    MethodHandle self = h("pthread_self", FunctionDescriptor.of(ValueLayout.ADDRESS));
    MethodHandle getattr = h("pthread_getattr_np", FunctionDescriptor.of(ValueLayout.JAVA_INT, ValueLayout.ADDRESS, ValueLayout.ADDRESS));
    MethodHandle getstack = h("pthread_attr_getstack", FunctionDescriptor.of(ValueLayout.JAVA_INT, ValueLayout.ADDRESS, ValueLayout.ADDRESS, ValueLayout.ADDRESS));
    MethodHandle destroy = h("pthread_attr_destroy", FunctionDescriptor.of(ValueLayout.JAVA_INT, ValueLayout.ADDRESS));
    long lo, size;
    try (Arena a = Arena.ofConfined()) {
      MemorySegment attr = a.allocate(256, 16), addr = a.allocate(8, 8), sz = a.allocate(8, 8);
      MemorySegment t = (MemorySegment) self.invokeExact();
      int r1 = (int) getattr.invokeExact(t, attr);
      int r2 = (int) getstack.invokeExact(attr, addr, sz);
      int r3 = (int) destroy.invokeExact(attr);
      lo = addr.get(ValueLayout.JAVA_LONG, 0); size = sz.get(ValueLayout.JAVA_LONG, 0);
      System.out.printf("getattr=%d getstack=%d destroy=%d lo=%x size=%d hi=%x%n", r1, r2, r3, lo, size, lo + size);
    }
    // warm so the recursion is compiled (the 112 B figure is a compiled frame)
    for (int i = 0; i < 20000; i++) deep(50);
    long[] a = snap(), b = deep(1000);
    for (int i = 0; i < a.length; i++) {
      long x = a[i], y = b[i];
      if (x >= lo && x < lo + size && y >= lo && y < lo + size && x != y)
        System.out.printf("offset %d: depth0=%x depth1000=%x delta=%d B (%.1f per frame)%n", i * 8, x, y, x - y, (x - y) / 1000.0);
    }
  }
}
