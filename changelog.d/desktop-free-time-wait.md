## desktop-free-time-wait - a port left in TIME_WAIT by the last copy is free

okay-watch fixed its own free-port check the same morning (aea8228 there:
a restart into a new version found its port refused by nothing but the
last process's closed connections, and moved to another). That check had
become okay-desktop's `Desktop.free` in app-in-process, so the fix moves
here too: a strict bind is still tried on every address and the loopback
alone, and a port that refuses it while nothing answers a connect is free.

- okay-desktop `Desktop.free`: `bindable || !listening(port)`.
- `TestFreePort` (Live, it binds ports): the TIME_WAIT case, okay-watch's
  test moved with it.
