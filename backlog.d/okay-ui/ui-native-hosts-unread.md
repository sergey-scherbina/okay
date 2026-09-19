- [ ] ui-native-hosts-unread — Swing, GTK, Compose, SwiftUI and the
      Android APK have each drawn the conformance script and a
      counter; none has drawn a product page. NARROWED 2026-09-18
      (native-tokens-tested): the tokens ui-text-intent gave them are
      no longer a claim — TestSwing asserts a monospaced identifier, a
      right-aligned `Align.End` label and the ABSENCE of tabular
      figures Swing cannot draw; TestGtk asserts the `monospace` and
      `numeric` style classes against real GTK widgets (a
      `gtk_widget_has_css_class` binding was added to read them back).
      What is still unread is a SCREEN: layout under a product's
      density, not a token. The recorded gaps
      (Swing: gap not drawn, weights as natural sizes; GTK: no
      weights, multiline as a plain entry, images as labels,
      `Gtk.window` never run with an app; Compose claims nothing;
      the iOS app bundle and the emulator run are still `[ ]` in
      specs/frontend.md) are honest and unranked, because nothing
      with a reader has ranked them. okay-watch found four things in
      two days on the browser; a native client will find its own. Not
      a lane — a record with a TRIGGER: the first product screen on a
      native host, which becomes that host's ui-product.
