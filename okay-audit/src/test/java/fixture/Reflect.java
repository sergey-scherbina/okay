package fixture;

/** the escape hatches */
public final class Reflect {
  public static Object go(String name, byte[] bytes) throws Exception {
    Class<?> c = Class.forName(name);
    java.lang.invoke.MethodHandles.Lookup l = java.lang.invoke.MethodHandles.lookup();
    Object o = new java.io.ObjectInputStream(new java.io.ByteArrayInputStream(bytes)).readObject();
    return c.getName() + l + o;
  }
}
