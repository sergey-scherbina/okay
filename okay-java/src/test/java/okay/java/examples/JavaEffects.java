package okay.java.examples;

import okay.java.Control;
import okay.java.Eff;
import okay.java.Handler;
import okay.java.Op;
import okay.java.StateHandler;
import okay.java.Stated;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

/**
 * okay effects and handlers written in Java (specs/java-effects.md): what a
 * Java user writes, kept to Java 17 — records, sealed interfaces and
 * {@code instanceof} patterns, no pattern {@code switch} — since that is the
 * floor okay-java runs on. {@code TestJavaEffects} runs each and asserts.
 */
public final class JavaEffects {
    private JavaEffects() {}

    // ------------------------------------------------------------ effects

    /** an effect: a sealed interface of records, each answering its R */
    public sealed interface Counter<R> extends Op<R> {
        record Next() implements Counter<Integer> {}
    }

    public sealed interface Log<R> extends Op<R> {
        record Line(String text) implements Log<Void> {}
    }

    /** nondeterminism: answered true AND false by a handler that resumes twice */
    public record Flip() implements Op<Boolean> {}

    /** an abort: a handler that never resumes */
    public record Fail(String why) implements Op<Void> {}

    // ------------------------------------------------------------ programs

    public static Eff<Integer> twoNexts() {
        return Eff.perform(new Counter.Next())
            .flatMap(a -> Eff.perform(new Counter.Next()).map(b -> a + b));
    }

    // form 1: each operation answered
    public static int answered() {
        return twoNexts().handle(Handler.answer(Counter.class, op -> 21)).run();
    }

    // form 2: a state threaded through the operations
    public static Stated<Integer, Integer> counted() {
        return twoNexts()
            .handle(StateHandler.of(Counter.class, 10, (n, op) -> Stated.of(n + 1, n)))
            .run();
    }

    // form 3: each operation a program in another effect — here the core State
    public static Stated<Integer, Integer> intoState() {
        return twoNexts()
            .handle(Handler.into(Counter.class, op -> Eff.<Integer>modify(n -> n + 1)))
            .handle(StateHandler.state(0))
            .run();
    }

    public static Eff<Integer> twoFlips() {
        return Eff.perform(new Flip())
            .flatMap(a -> Eff.perform(new Flip()).map(b -> (a ? 1 : 0) + (b ? 2 : 0)));
    }

    // form 4: the continuation in hand, resumed twice
    public static List<Integer> allFlips() {
        Control<Integer, List<Integer>> all = Control.of(Flip.class,
            (Integer a) -> Eff.pure(List.of(a)),
            (op, k) -> k.resume(true).flatMap(xs -> k.resume(false).map(ys -> concat(xs, ys))));
        return twoFlips().handle(all).run();
    }

    // form 4: never resumed — the rest of the program is dropped
    public static Optional<Integer> failing(boolean fail) {
        Eff<Integer> p = Eff.pure(1)
            .flatMap(a -> fail ? Eff.perform(new Fail("no")).map(u -> a) : Eff.pure(a))
            .map(a -> a + 1);
        Control<Integer, Optional<Integer>> abort = Control.of(Fail.class,
            (Integer a) -> Eff.pure(Optional.of(a)),
            (op, k) -> Eff.pure(Optional.empty()));
        return p.handle(abort).run();
    }

    // nothing handles Counter: run refuses it by name
    public static int unhandled() {
        return twoNexts().run();
    }

    /** two Java effects in one program */
    public static Eff<Integer> countAndLog() {
        return Eff.perform(new Counter.Next())
            .flatMap(a -> Eff.perform(new Log.Line("got " + a))
            .andThen(Eff.perform(new Counter.Next()))
            .flatMap(b -> Eff.perform(new Log.Line("got " + b)).map(u -> a + b)));
    }

    static StateHandler<List<String>> lines() {
        return StateHandler.of(Log.class, List.<String>of(),
            (xs, op) -> Stated.of(concat(xs, List.of(((Log.Line) op).text())), null));
    }

    static StateHandler<Integer> counter() {
        return StateHandler.of(Counter.class, 1, (n, op) -> Stated.of(n + 1, n));
    }

    public static String logInside() {
        Stated<Integer, Stated<List<String>, Integer>> r = countAndLog().handle(lines()).handle(counter()).run();
        return r.state() + " " + r.value().state() + " " + r.value().value();
    }

    public static String logOutside() {
        Stated<List<String>, Stated<Integer, Integer>> r = countAndLog().handle(counter()).handle(lines()).run();
        return r.value().state() + " " + r.state() + " " + r.value().value();
    }

    // ------------------------------------------------------------ the core effects

    public static Eff<String> config() {
        return Eff.<Integer>ask()
            .flatMap(env -> Eff.<Integer>modify(s -> s + env))
            .flatMap(s -> s > 100 ? Eff.<String>raise("too big: " + s) : Eff.pure("s=" + s));
    }

    public static Stated<Integer, String> core(int env) {
        return config()
            .recover(e -> "recovered " + e)
            .handle(StateHandler.state(1))
            .handle(Handler.reader(env))
            .run();
    }

    public static int async() {
        return Eff.sleep(5).andThen(Eff.async(() -> 40)).map(n -> n + 2).runAsync();
    }

    // ------------------------------------------------------------ stack safety

    /** a Java loop as recursion, 1 000 000 deep: constant stack, each step a State operation */
    static Eff<Integer> steps(int n) {
        if (n == 0) return Eff.get();
        return Eff.<Integer>modify(s -> s + 1).flatMap(s -> steps(n - 1));
    }

    public static int deepState(int n) {
        return steps(n).handle(StateHandler.state(0)).run().value();
    }

    /** a plain tail call: without {@code defer} this is a StackOverflowError */
    static Eff<Long> down(long n, long acc) {
        if (n == 0) return Eff.pure(acc);
        return Eff.defer(() -> down(n - 1, acc + n));
    }

    public static long deepDefer(long n) {
        return down(n, 0).run();
    }

    // ------------------------------------------------------------ helpers

    static <T> List<T> concat(List<T> a, List<T> b) {
        List<T> out = new ArrayList<>(a);
        out.addAll(b);
        return out;
    }
}
