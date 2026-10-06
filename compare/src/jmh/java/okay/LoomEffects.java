package okay;

import jdk.internal.vm.Continuation;
import jdk.internal.vm.ContinuationScope;

/**
 * A direct-style handler on the JVM's OWN one-shot continuations — the
 * machinery under virtual threads, used as a delimited continuation
 * (specs/java-direct-effects.md): the scope is the prompt, {@code yield} is
 * the operation's shift, {@code run()} resumes with the answer. Internal and
 * unsupported: compiling and running it needs
 * {@code --add-exports java.base/jdk.internal.vm=ALL-UNNAMED}.
 *
 * <p>Java because the API is in a package the JDK does not export, and
 * scalac's {@code -java-output-version} reads only the exported API.
 */
public final class LoomEffects {
    private static final ContinuationScope SCOPE = new ContinuationScope("okay-effects");
    private static final Object NEXT = new Object();

    private Object pending;
    private int answer;

    /** the operation: hand it to the handler, wait for its answer */
    private int perform(Object op) {
        pending = op;
        Continuation.yield(SCOPE);
        return answer;
    }

    /** {@code n} operations, each answered 1 by the handler loop: the sum, n */
    public static int run(int n) {
        LoomEffects h = new LoomEffects();
        int[] out = new int[1];
        Continuation body = new Continuation(SCOPE, () -> {
            int acc = 0;
            for (int i = 0; i < n; i++) acc += h.perform(NEXT);
            out[0] = acc;
        });
        while (true) {
            body.run();
            if (body.isDone()) return out[0];
            if (h.pending != NEXT) throw new IllegalStateException("not an operation: " + h.pending);
            h.pending = null;
            h.answer = 1;
        }
    }
}
