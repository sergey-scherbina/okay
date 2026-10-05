package okay.durable

private[durable] object RunId:
  def fresh(): String = java.util.UUID.randomUUID().toString
