package okay.java.examples;

import okay.java.Cap;
import okay.java.Eff;
import okay.java.Env;
import okay.java.Io;
import okay.java.Op;
import okay.java.Raise;
import okay.java.Stated;
import okay.java.Var;

import java.util.ArrayList;
import java.util.List;

/**
 * The static row for Java (specs/java-capabilities.md): every program below
 * names the effects it needs as PARAMETERS, and only a handler can supply
 * one. {@code TestJavaCapabilities} runs each and asserts. Java 17 source.
 */
public final class JavaCapabilities {
    private JavaCapabilities() {}

    // ------------------------------------------------------------ an effect of our own

    public sealed interface CounterOp<R> extends Op<R> {
        record Next() implements CounterOp<Integer> {}
    }

    /** the effect's typed face: what a program's signature names */
    public record Counter(Cap<CounterOp> cap) {
        public Eff<Integer> next() { return cap.perform(new CounterOp.Next()); }
    }

    /** its row is its parameter: no Counter, no call */
    static Eff<Integer> two(Counter c) {
        return c.next().flatMap(a -> c.next().map(b -> a + b));
    }

    // form 1
    public static int answered() {
        return Cap.answer(CounterOp.class, op -> 21, c -> two(new Counter(c))).run();
    }

    // form 2
    public static Stated<Integer, Integer> counted() {
        return Cap.state(CounterOp.class, 10, (n, op) -> Stated.of(n + 1, n), c -> two(new Counter(c))).run();
    }

    // form 3: the Counter translated into a Var
    public static Stated<Integer, Integer> intoVar() {
        return Var.run(0, (Var<Integer> v) ->
            Cap.into(CounterOp.class, op -> v.modify(n -> n + 1), c -> two(new Counter(c)))).run();
    }

    // form 4: every answer of two flips
    public record Flip() implements Op<Boolean> {}

    static Eff<Integer> twoFlips(Cap<Flip> f) {
        return f.perform(new Flip()).flatMap(a -> f.perform(new Flip()).map(b -> (a ? 1 : 0) + (b ? 2 : 0)));
    }

    public static List<Integer> allFlips() {
        return Cap.control(Flip.class,
            (Integer a) -> Eff.pure(List.of(a)),
            (op, k) -> k.resume(true).flatMap(xs -> k.resume(false).map(ys -> concat(xs, ys))),
            JavaCapabilities::twoFlips).run();
    }

    // ------------------------------------------------------------ two instances of one effect

    public record Ask() implements Op<Integer> {}

    /** the inner handler must not take the outer capability's operation */
    public static int nested() {
        return Cap.answer(Ask.class, op -> 1, a ->
               Cap.answer(Ask.class, op -> 2, b ->
                   a.perform(new Ask()).flatMap(x -> b.perform(new Ask()).map(y -> x * 10 + y)))).run();
    }

    public static String twoInts() {
        Stated<Integer, Stated<Integer, Integer>> r =
            Var.run(10, (Var<Integer> a) -> Var.run(20, (Var<Integer> b) ->
                a.modify(x -> x + 1).andThen(b.modify(x -> x + 2)).andThen(a.get()))).run();
        return r.state() + " " + r.value().state() + " " + r.value().value();
    }

    public static String intAndString() {
        Stated<Integer, Stated<String, String>> r =
            Var.run(0, (Var<Integer> n) -> Var.run("", (Var<String> s) ->
                n.modify(x -> x + 1).flatMap(x -> s.modify(t -> t + x))
                    .andThen(n.modify(x -> x + 1)).flatMap(x -> s.modify(t -> t + x)))).run();
        return r.state() + " " + r.value().state();
    }

    // ------------------------------------------------------------ the built-ins together

    static Eff<String> config(Env<Integer> env, Var<Integer> st, Raise<String> err) {
        return env.ask()
            .flatMap(e -> st.modify(s -> s + e))
            .flatMap(s -> s > 100 ? err.<String>raise("too big: " + s) : Eff.pure("s=" + s));
    }

    public static Stated<Integer, String> builtIns(int env) {
        return Var.run(1, (Var<Integer> st) ->
            Env.run(env, (Env<Integer> e) ->
                Raise.recover((String msg) -> "recovered " + msg, (Raise<String> err) -> config(e, st, err)))).run();
    }

    public static int io() {
        return Io.run(io -> io.sleep(5).andThen(io.async(() -> 40)).map(n -> n + 2));
    }

    // ------------------------------------------------------------ escape

    /** a capability kept past its handler, then used */
    public static int escaped() {
        List<Cap<Ask>> leak = new ArrayList<>();
        Cap.answer(Ask.class, op -> 1, a -> { leak.add(a); return Eff.pure(0); }).run();
        return leak.get(0).perform(new Ask()).run();
    }

    /** the same, used inside a NEW handler of the same effect: not its capability, not its operation */
    public static int escapedIntoAnother() {
        List<Cap<Ask>> leak = new ArrayList<>();
        Cap.answer(Ask.class, op -> 1, a -> { leak.add(a); return Eff.pure(0); }).run();
        return Cap.answer(Ask.class, op -> 2, b -> leak.get(0).perform(new Ask())).run();
    }

    // ------------------------------------------------------------ stack safety

    static Eff<Integer> steps(Var<Integer> v, int n) {
        if (n == 0) return v.get();
        return v.modify(s -> s + 1).flatMap(s -> steps(v, n - 1));
    }

    public static int deep(int n) {
        return Var.run(0, (Var<Integer> v) -> steps(v, n)).run().value();
    }

    static <T> List<T> concat(List<T> a, List<T> b) {
        List<T> out = new ArrayList<>(a);
        out.addAll(b);
        return out;
    }
}
