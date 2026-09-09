/*
 * What ends a Scala Native test process, and what the runner says about
 * it (native-runner-cause, 2026-09-09).
 *
 * BACKLOG's `native-runner-error` had a Native module fail with no test
 * failed, three times in one day, and read the symptom as "a lost test
 * process" under memory pressure. The third occurrence carried a
 * different exception and no `Errors 1` line at all, so the reading was
 * re-derived from the runner's own sources — and then MEASURED here,
 * because a source reading is a hypothesis.
 *
 * This harness drives a real Native test binary exactly the way
 * sbt-scala-native's ComRunner does — a ServerSocket on port 0, the
 * binary spawned with that port as its only argument, an accept — and
 * then ends the connection four ways, printing the exit code and
 * everything the binary printed.
 *
 * Measured 2026-09-09 on okay-lex/.native/target/scala-3.9.0/okay-lex-test:
 *
 *   close-socket  exit=0    printed nothing
 *   zero-length   exit=0    printed nothing
 *   sigterm       exit=143  printed nothing
 *   sigkill       exit=137  printed nothing
 *
 * Why the map settles the question: on a non-zero exit ProcessRunner
 * fails its promise with "Process ... finished with non-zero value N",
 * ComRunner logs "Force close ..." and, above 128, the runner also logs
 * "Test runner interrupted by fatal signal N" — and the RunTerminated-
 * Exception then CARRIES that failure as its cause. A gate log with
 * none of those three lines, and a RunTerminatedException with no
 * cause, is therefore a process that exited ZERO: nobody killed it, its
 * connection ended. TestMain's loop returns 0 on exactly two inputs,
 * both reproduced above: end of stream, or a message length <= 0.
 *
 * Usage:  java scripts/native-runner-probe.java [path-to-native-test-binary]
 * Default: okay-lex/.native/target/scala-3.9.0/okay-lex-test, which
 * `sbt okayLexNative/test` builds.
 */
import java.io.*;
import java.net.*;
import java.nio.file.*;
import java.util.concurrent.*;

public class NativeRunnerProbe {

  static final String DEFAULT_BINARY =
      "okay-lex/.native/target/scala-3.9.0/okay-lex-test";

  public static void main(String[] args) throws Exception {
    String bin = args.length > 0 ? args[0] : DEFAULT_BINARY;
    if (!Files.isExecutable(Paths.get(bin))) {
      System.err.println("no such binary: " + bin);
      System.err.println("build one with: sbt okayLexNative/test");
      System.exit(2);
    }
    System.out.println("probing " + bin);
    for (String how : new String[] {"close-socket", "zero-length", "sigterm", "sigkill"})
      run(bin, how);
  }

  /** spawn the binary as ComRunner does, let it connect, then end the
   *  connection the way `how` says and report what the binary did */
  static void run(String bin, String how) throws Exception {
    ServerSocket server = new ServerSocket(0, 1);
    ProcessBuilder pb = new ProcessBuilder(bin, String.valueOf(server.getLocalPort()));
    pb.redirectErrorStream(true);
    Process p = pb.start();

    ByteArrayOutputStream out = new ByteArrayOutputStream();
    Thread pump = new Thread(() -> {
      try { p.getInputStream().transferTo(out); } catch (IOException ignored) {}
    });
    pump.setDaemon(true);
    pump.start();

    server.setSoTimeout(40_000); // ComRunner's own timeout
    Socket com = server.accept();
    server.close();
    Thread.sleep(300); // connected, waiting for a call — as in the real failure

    switch (how) {
      case "close-socket" -> com.close();
      case "zero-length" -> {
        DataOutputStream d = new DataOutputStream(com.getOutputStream());
        d.writeInt(0);
        d.flush();
      }
      case "sigterm" -> p.destroy();
      case "sigkill" -> p.destroyForcibly();
      default -> throw new IllegalArgumentException(how);
    }

    boolean ended = p.waitFor(20, TimeUnit.SECONDS);
    int code = ended ? p.exitValue() : -1;
    pump.join(1000);
    String printed = out.toString().trim();
    System.out.printf("  %-13s exit=%-5s %-52s printed: %s%n",
        how,
        ended ? String.valueOf(code) : "HUNG",
        code > 128 ? "(signal " + (code - 128) + " — the runner LOGS it, with a cause)"
            : code == 0 ? "(clean — RunTerminatedException, NO cause, silent)"
            : "(non-zero — the runner LOGS it, with a cause)",
        printed.isEmpty() ? "nothing" : printed.replace('\n', ' '));

    if (!ended) p.destroyForcibly();
    try { com.close(); } catch (IOException ignored) {}
  }
}
