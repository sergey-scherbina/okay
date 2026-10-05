package fixture;

import java.util.function.Function;

/** a lambda, a string concatenation, a record: three bootstraps, zero findings */
public final class Lambdas {
  public record Pair(int a, String b) {}
  public static String go(int n) {
    Function<Integer, Integer> f = x -> x + 1;
    return "n=" + f.apply(n) + new Pair(n, "x");
  }
}
