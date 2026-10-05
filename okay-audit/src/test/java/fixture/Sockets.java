package fixture;

/** a business class that reaches the network directly */
public final class Sockets {
  public static int port() throws Exception {
    try (java.net.Socket s = new java.net.Socket("localhost", 80)) { return s.getPort(); }
  }
}
