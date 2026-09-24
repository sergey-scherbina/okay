package okay.pool

import okay.codec.Schema

/**
 * WHAT THE HTTP DOOR SAYS (specs/cluster-pool.md, stage 1). `value`
 * is `okay.cluster.Job.Answer`'s own already-JSON string, carried
 * straight through — a caller who wants it as a structured value
 * re-parses it under the job's own answer shape, which this door
 * cannot name (`Jobs.find` hands back an existential `Job[?, ?]`; see
 * `Job.answer`'s doc comment for why).
 */
final case class Submitted(run: String) derives Schema

enum Status derives Schema:
  case Running(epoch: Int, peers: Int)
  case Done(value: String, dropped: Long, merged: Long, retried: Long, failed: Long)
  case Failed(why: String)

/** the shape every JSON error body takes, so a caller parses one way */
private[pool] final case class ErrorBody(error: String) derives Schema
