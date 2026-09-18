package okay.cluster

import okay.codec.{Compat, Schema}

/**
 * THE LEAK, READ (specs/federation.md, stage 4).
 *
 * Claim 3, made mechanical: "an aggregate can still be a record. A
 * count of one, a max over one element, a key that identifies a
 * person — these are records with a different name, and this spec
 * says so rather than claiming otherwise. What the engine offers is
 * that the LEAK IS THE SCHEMA: it can be read, reasoned about and
 * bounded before a job runs, because nothing else crosses."
 *
 * This is the reading. It does not decide whether a job is safe to
 * federate — that needs what a field IS, which the Schema does not
 * carry — it prints what a job's `Wire#wire` lets out, per key where
 * there is one, and flags the two shapes Claim 3 names by name:
 *
 *   - a KEY whose type has no bound on how many values it can take
 *     (a `String`, raw `Bytes`) — an email address is exactly this
 *     shape, and so is a two-letter country code; the Schema cannot
 *     tell them apart, which is why this is a thing to READ rather
 *     than a verdict;
 *   - a VALUE that is a bare scalar (`Int`, `Long`, `String`, ...)
 *     with NOTHING beside it — no field, no sibling — that could say
 *     how many records went into it. A group of one, or a fold over
 *     one event, is then indistinguishable on the wire from that
 *     record's own field.
 *
 * A COMPOUND value (a user's own `Aggregator`'s accumulator, whatever
 * shape it derives) is reported by name and left there: whether its
 * OWN fields answer "how many" is a question about what they MEAN,
 * which is not a Schema's business — this tool exposes the shape and
 * stops, as the spec asks.
 */
object Leak:

  /** where a field sits in what crosses: `Key` — one of the fields
   * that identifies a group (a `Wire.keyed` or `Wire.windowed`
   * partial's key, or a window's start); `Value` — what a group's
   * members were folded into, or a `Wire.fold` partial's whole
   * accumulator, which crosses once per PARTITION rather than once
   * per key and asks the same question at that grain instead */
  enum Role:
    case Key, Value

  /** one thing that crosses, described rather than judged */
  final case class Field(path: String, shape: String, role: Role)

  final case class Report(fields: Vector[Field]):
    /** Claim 3's "a count of one, a max over one element": a bare
     * scalar value has nothing beside it to say how many records
     * made it. A compound value at least hands the operator field
     * names to read (`uncounted` does not look inside one) — the
     * mechanical part of the claim stops at the shape. */
    def uncounted: Vector[Field] = fields.filter(f => f.role == Role.Value && SCALAR(f.shape))

    /** Claim 3's "a key that identifies a person": a key whose shape
     * has no bound on its cardinality. */
    def unboundedKeys: Vector[Field] = fields.filter(f => f.role == Role.Key && UNBOUNDED(f.shape))

    def isEmpty: Boolean = fields.isEmpty

    /** the operator's page: every field that crosses, then the two
     * flagged lists, so a job with neither is visibly clean */
    def render: String =
      val sb = StringBuilder()
      if fields.isEmpty then sb ++= "nothing crosses\n"
      else
        sb ++= s"${fields.length} field(s) cross:\n"
        for f <- fields do
          val tag =
            if f.role == Role.Value && SCALAR(f.shape) then " [UNCOUNTED]"
            else if f.role == Role.Key && UNBOUNDED(f.shape) then " [UNBOUNDED]"
            else ""
          sb ++= s"  ${f.role} ${f.path}: ${f.shape}$tag\n"
      if uncounted.nonEmpty then
        sb ++= s"\n${uncounted.length} value(s) with no count beside them — a group of one is silently a raw record:\n"
        for f <- uncounted do sb ++= s"  ${f.path}: ${f.shape}\n"
      if unboundedKeys.nonEmpty then
        sb ++= s"\n${unboundedKeys.length} key(s) with unbounded cardinality — read what they identify:\n"
        for f <- unboundedKeys do sb ++= s"  ${f.path}: ${f.shape}\n"
      sb.result()

  private val SCALAR = Set("Int", "Long", "Double", "Boolean", "Char", "String", "Bytes")
  private val UNBOUNDED = Set("String", "Bytes")

  /**
   * THE NAMES `okay.cluster.Wire`'s OWN COMPOSITION HELPERS GIVE A
   * PRODUCT — `pair`/`triple` (the key(s)-then-value convention every
   * `Wire.keyed`/`Wire.windowed` partial is built from) and `handed`
   * (`Sink.staging`'s boundary/finished/late). These are STRUCTURAL
   * ENVELOPES: each of their fields is an independently crossing
   * thing in its own right, so `walk` opens them.
   *
   * Every OTHER product — a user's own `Aggregator`'s accumulator,
   * `derives Schema` and all — is read as ONE aggregate value:
   * naming it is as far as this tool goes, on purpose. Fragmenting it
   * into its own leaves would report `Sum.n`, `Sum.total`, `Sum.x` as
   * three independent "leaks" instead of the one accumulator they
   * are, which is worse than useless — it buries the one finding that
   * matters (a bare scalar with nothing beside it) under noise from
   * every accumulator that already carries its own explanation.
   */
  private val ENVELOPES = Set("Pair", "Triple", "Handed")

  private def under(s: Schema[?]): Schema[?] = s match
    case Schema.SIso(u, _, _) => under(u())
    case other => other

  private def at(path: String, name: String): String = if path.isEmpty then name else s"$path.$name"

  /**
   * WHAT A JOB'S PARTIAL LETS OUT (specs/federation.md, stage 4).
   *
   * `wire` is `Wire#wire` — the Schema every partial that crosses a
   * wire is described by (`Resp.Partial`'s bytes, `TestFederation`'s
   * `accumulatorsOnly` decodes exactly this). Nothing else can leave a
   * party; this reads what that Schema says leaves.
   */
  def of(wire: Schema[?]): Report = Report(walk(wire, ""))

  private def walk(s: Schema[?], path: String): Vector[Field] = under(s) match
    case Schema.SOption(of) => walk(of(), path)
    case Schema.SList(of) => group(of(), path)
    case Schema.SVector(of) => group(of(), path)
    case p: Schema.SProduct[?] if ENVELOPES(p.name) =>
      p.fields.flatMap((n, sc) => walk(sc(), at(path, n)))
    case su: Schema.SSum[?] => su.cases.flatMap((n, sc) => walk(sc(), at(path, n)))
    case other =>
      // AN UNKEYED VALUE: a `Wire.fold` partial's whole accumulator
      // (bare, or a user's own product — either way opaque), or a
      // plain sibling field like `Handed.late`. It crosses once per
      // PARTITION rather than once per key, and the same "could this
      // be one record's own value" question applies at that grain —
      // a fold over a partition with one event is exactly as exposed
      // as a keyed group with one member.
      Vector(Field(path, Compat.shape(other), Role.Value))

  /**
   * ONE GROUP: a `Vector`/`List` whose element is a product of two or
   * more fields. `Wire.keyed`'s `pair(K, Acc)` and `Wire.windowed`'s
   * `triple(start, K, Acc)` are built by this exact convention — key
   * fields first, the fold's own accumulator last — because that is
   * the shape every one of this engine's keyed and windowed partials
   * already has. A hand-rolled `Vector[(K, V)]` from `derives Schema`
   * has the identical shape by construction, so nothing here is
   * specific to those two helpers by name (unlike `walk`'s envelope
   * check, which is).
   */
  private def group(elem: Schema[?], path: String): Vector[Field] =
    under(elem) match
      case p: Schema.SProduct[?] if p.fields.length >= 2 =>
        val (valueName, valueThunk) = p.fields.last
        val keyFields = p.fields.dropRight(1).map((n, sc) =>
          Field(at(path, n), Compat.shape(sc()), Role.Key))
        keyFields :+ Field(at(path, valueName), Compat.shape(valueThunk()), Role.Value)
      case other =>
        // no key visible in this group's own element — every entry is
        // itself the value (a `Vector[Long]` fold intermediate, say)
        Vector(Field(path, Compat.shape(other), Role.Value))
