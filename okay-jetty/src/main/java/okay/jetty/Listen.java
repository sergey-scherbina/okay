package okay.jetty;

import org.eclipse.jetty.websocket.api.Callback;
import org.eclipse.jetty.websocket.api.Session;
import java.nio.ByteBuffer;

/**
 * Jetty's session listener, in Java — and the reason is worth stating,
 * because it is not squeamishness about the language.
 *
 * Jetty decides which callbacks a listener wants by REFLECTION over the
 * methods its class declares, and it refuses a listener that declares
 * both `onWebSocketText(String)` and `onWebSocketPartialText(String,
 * boolean)` — you get one or the other, not both. Scala 3 emits mixin
 * forwarders for every default method of an implemented Java interface,
 * so a Scala listener declares them all, and Jetty rejects it with
 * "Cannot replace previously assigned [TEXT Handler]".
 *
 * Ten lines of Java declare exactly the four callbacks we want. The
 * Scala side implements `Sink`, which has no default methods and so has
 * nothing to forward.
 */
public final class Listen implements Session.Listener.AutoDemanding {

  /** what the Scala side implements — all abstract, nothing to forward */
  public interface Sink {
    void open(Session session);
    void text(String message);
    void binary(byte[] payload);
    void closed(int code, String reason);
    void failed(Throwable cause);
  }

  private final Sink sink;

  public Listen(Sink sink) { this.sink = sink; }

  @Override public void onWebSocketOpen(Session session) { sink.open(session); }

  @Override public void onWebSocketText(String message) { sink.text(message); }

  @Override public void onWebSocketBinary(ByteBuffer payload, Callback callback) {
    byte[] a = new byte[payload.remaining()];
    payload.get(a);
    sink.binary(a);
    callback.succeed();
  }

  @Override public void onWebSocketClose(int statusCode, String reason) {
    sink.closed(statusCode, reason == null ? "" : reason);
  }

  @Override public void onWebSocketError(Throwable cause) { sink.failed(cause); }
}
