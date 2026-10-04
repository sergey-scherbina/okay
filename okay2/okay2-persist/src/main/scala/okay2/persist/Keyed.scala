package okay2.persist

/** the one walk the keyed side tables share (Timers, Cancels, Leases,
 * Children, Statuses): every record of a snapshots topic, every
 * partition, oldest first — the latest per key is what the caller keeps */
private[persist] object Keyed {
  def foreach(t: Topic)(f: Record => Unit): Unit = {
    var p = 0
    while (p < t.partitions) {
      var from = t.begin(p)
      var going = true
      while (going) {
        t.read(p, from, 512) match {
          case Topic.Read.TooEarly(b) => from = b
          case Topic.Read.Records(rs) =>
            if (rs.isEmpty) going = false
            else {
              rs.foreach(f)
              from = rs.last.offset + 1
            }
        }
      }
      p += 1
    }
  }
}
