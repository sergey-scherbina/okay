- [ ] script-tls: ALPN/HTTP2, OCSP stapling, cipher policy — still the
      proxy's, and named as such in the spec. A Site behind Caddy/nginx/an ingress needs
      three things from the operator: pass Upgrade for EVERY path
      (a live page's socket is on the page's own path),
      `OKAY_FORWARDED=1`, and to treat `X-Forwarded-For` as a claim.
