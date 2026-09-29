- [ ] app-in-process — the app's window without a port (specs/app-in-process.md;
      operator, 2026-09-29: «проблема с необходимостью угадывать порт внутри
      приложения — это разве не абсурд?»). okay-desktop: `InProcess.Server`
      and the `app://` scheme (reads), the bridge's `send`/`open` and the held
      answer (writes), `Transport` for the window's own requests, `Instance`
      (a lock and a Unix-domain socket in the data folder), `launch` with
      `Mode.InWindow`/`Mode.OnPort`; okay-ui: `Enhance` sends through the
      bridge when there is one. Gate: the new suites, `affected master
      Test/compile`, and the window under Xvfb with screenshots.
