package fixture;

public final class Clocky {
  public static long now() { return System.currentTimeMillis() + new java.util.Date().getTime(); }
  public static int die() { return new java.util.Random().nextInt(); }
}
