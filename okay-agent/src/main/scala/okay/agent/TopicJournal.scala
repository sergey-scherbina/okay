package okay.agent

/** Source-compatible name; the stored version-1 records are unchanged. */
type TopicJournal = okay.durable.persist.TopicJournal

object TopicJournal:
  export okay.durable.persist.TopicJournal.Rec

  def apply(topic: okay.persist.Topic, run: String): TopicJournal =
    new okay.durable.persist.TopicJournal(topic, run)
