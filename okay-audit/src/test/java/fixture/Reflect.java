package fixture;

/** the escape hatches */
public final class Reflect {
  public static Object go(String name, byte[] bytes) throws Exception {
    Class<?> c = Class.forName(name);
    java.lang.invoke.MethodHandle h = java.lang.invoke.MethodHandles.lookup()
      .findStatic(c, name, java.lang.invoke.MethodType.methodType(void.class));
    Object o = new java.io.ObjectInputStream(new java.io.ByteArrayInputStream(bytes)).readObject();
    return c.getName() + h + o;
  }
}
