- [x] offload-getbytes — DONE 2026-09-16 (producer-drains). Was:
      `Offload.fetchBytes` is `Blob.getBytes` with
      a throw on the Left: a fourth copy of the walk `Producer.each`
      replaced in Backup, twenty lines that are now one call. Lands
      with producer-drains.
