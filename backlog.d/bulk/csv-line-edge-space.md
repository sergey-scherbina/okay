- [ ] csv-line-edge-space — `Csv.line` quotes a field with a comma, a
      quote, CR or LF; it does not quote one with a LEADING or TRAILING
      space, which Excel trims when unquoted (and `fields` then reads
      back a different value from what Excel shows). okay-watch's
      writer (`okaywatch.Sheet`, 2026-09-25) is `Csv.line` plus that one
      rule and would be `Csv.line` itself with it. Also: `line` of a
      `null` field throws; okay-watch's treats it as empty.
