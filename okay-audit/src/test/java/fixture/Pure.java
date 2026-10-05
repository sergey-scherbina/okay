package fixture;

import java.util.List;

/** plain computation: collections, strings, math — nothing past the boundary */
public final class Pure {
  public static int sum(List<Integer> xs) {
    int s = 0;
    for (int x : xs) s += Math.abs(x);
    return s;
  }
}
