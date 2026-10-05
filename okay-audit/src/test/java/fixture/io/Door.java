package fixture.io;

/** the one class of a business module that is its handler layer: a package, not a project */
public final class Door {
  public static String fetch(String host) throws Exception {
    try (java.net.Socket s = new java.net.Socket(host, 80)) { return s.getInetAddress().getHostName(); }
  }
}
