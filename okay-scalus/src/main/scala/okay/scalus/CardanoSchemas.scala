package okay.scalus

import okay.codec.Schema
import io.bullet.borer.Encoder
import scala.collection.immutable.{SortedMap, SortedSet}
import scalus.uplc.builtin.{ByteString, Data}
import scalus.cardano.address.{Address, StakeAddress}
import scalus.cardano.ledger.*
import scalus.cardano.onchain.plutus.prelude.List as PList

/**
 * `Schema` instances for scalus's ledger model (specs/scalus.md §3), so a
 * block or a transaction folds into JSON, CBOR, a validator, engine-free
 * `Columns`, and through them a Spark DataFrame — one derivation for
 * every consumer, okay-watch included.
 *
 * Case classes and enums derive through their Mirrors; what needs a hand
 * here is what scalus made opaque or wraps:
 *
 * - `ByteString` and the `Hash` family → bytes;
 * - `KeepRaw[A]`/`Sized[A]` → the `A` (the original bytes are a table's
 *   `cbor` column where a chain hash is over them, never every node);
 * - the `Tagged*` sets and maps, `IndexedSeq`, `Map`, `SortedMap`,
 *   scalus's own prelude `List` → `Vector`s (of pairs, for maps);
 * - `Coin`, `Slot`, `Word64` → `Long`;
 * - `MultiAsset` → one `(policy, name, quantity)` triple per asset — the
 *   shape an `explode` wants, not a map of maps (§4.5);
 * - `Address` → its canonical text (bech32, base58 for Byron), because
 *   that is what people search by.
 *
 * `Data`, `Timelock` and `Metadatum` are recursive; `Columns` finds that
 * by itself and reads them as `cbor` + json.
 *
 * Use: `import okay.scalus.CardanoSchemas.given`.
 */
object CardanoSchemas:

  // ---- leaves ----------------------------------------------------------

  given Schema[ByteString] = Schema.wrap[ByteString, Array[Byte]](ByteString.fromArray, _.bytes)

  given [HF: HashSize, P]: Schema[Hash[HF, P]] =
    Schema.wrap[Hash[HF, P], Array[Byte]](b => Hash[HF, P](ByteString.fromArray(b)), _.bytes)

  given Schema[Coin] = Schema.wrap[Coin, Long](Coin(_), _.value)
  given Schema[Slot] = Schema.wrap[Slot, Long](Slot(_), _.slot)
  given Schema[Word64] = Schema.wrap[Word64, Long](Word64(_), _.value)

  given Schema[Address] = Schema.refine[Address, String](
    s => scala.util.Try(Address.fromString(s)).toEither.left.map(e => s"not an address: '$s' (${e.getMessage})"),
    a => a.encode.getOrElse(a.toBytes.toHex))

  given Schema[StakeAddress] = Schema.refine[StakeAddress, String](
    s => scala.util.Try(Address.fromString(s)).toEither.left.map(e => s"not an address: '$s' (${e.getMessage})")
      .flatMap {
        case sa: StakeAddress => Right(sa)
        case _ => Left(s"'$s' is not a stake address")
      },
    a => a.encode.getOrElse(a.toBytes.toHex))

  // ---- wrappers the model keeps its bytes in ---------------------------

  given [A](using s: Schema[A], e: Encoder[A]): Schema[KeepRaw[A]] =
    Schema.wrap[KeepRaw[A], A](KeepRaw(_), _.value)

  given [A](using s: Schema[A], e: Encoder[A]): Schema[Sized[A]] =
    Schema.wrap[Sized[A], A](Sized(_), _.value)

  // ---- collections -----------------------------------------------------

  given [A](using s: Schema[A]): Schema[IndexedSeq[A]] =
    Schema.wrap[IndexedSeq[A], Vector[A]](_.toIndexedSeq, _.toVector)

  given [A](using s: Schema[A]): Schema[PList[A]] =
    Schema.wrap[PList[A], Vector[A]](PList.from(_), _.toScalaList.toVector)

  given [A](using s: Schema[A]): Schema[Set[A]] =
    Schema.wrap[Set[A], Vector[A]](_.toSet, _.toVector)

  given [A: Ordering](using s: Schema[A]): Schema[SortedSet[A]] =
    Schema.wrap[SortedSet[A], Vector[A]](SortedSet.from(_), _.toVector)

  given [A: Ordering](using s: Schema[A]): Schema[TaggedSortedSet[A]] =
    Schema.wrap[TaggedSortedSet[A], Vector[A]](TaggedSortedSet.from(_), _.toSeq.toVector)

  given [A](using s: Schema[A]): Schema[TaggedOrderedSet[A]] =
    Schema.wrap[TaggedOrderedSet[A], Vector[A]](TaggedOrderedSet.from(_), _.toSeq.toVector)

  /** scalus's `from` REFUSES an empty set and duplicates (Conway rules) by
   * throwing; here empty is `empty`, and a duplicate is a decode error */
  given [A](using s: Schema[A]): Schema[TaggedOrderedStrictSet[A]] =
    Schema.refine[TaggedOrderedStrictSet[A], Vector[A]](
      xs => if xs.isEmpty then Right(TaggedOrderedStrictSet.empty[A])
            else scala.util.Try(TaggedOrderedStrictSet.from(xs)).toEither.left.map(_.getMessage),
      _.toSeq.toVector)

  given [K, V](using k: Schema[K], v: Schema[V]): Schema[Map[K, V]] =
    Schema.wrap[Map[K, V], Vector[(K, V)]](_.toMap, _.toVector)

  given [K: Ordering, V](using k: Schema[K], v: Schema[V]): Schema[SortedMap[K, V]] =
    Schema.wrap[SortedMap[K, V], Vector[(K, V)]](SortedMap.from(_), _.toVector)

  /** scalus's tagged maps derive each KEY from its value (`KeyOf`: a
   * script's hash, a datum's hash), so the wire carries the values alone
   * and `from` rebuilds the keys — nothing can disagree */
  given [K: Ordering, V](using v: Schema[V], key: TaggedSortedMap.KeyOf[K, V]): Schema[TaggedSortedMap[K, V]] =
    Schema.wrap[TaggedSortedMap[K, V], Vector[V]](TaggedSortedMap.from(_), _.toSortedMap.values.toVector)

  given [K: Ordering, V](using v: Schema[V], key: TaggedSortedStrictMap.KeyOf[K, V]): Schema[TaggedSortedStrictMap[K, V]] =
    Schema.refine[TaggedSortedStrictMap[K, V], Vector[V]](
      xs => scala.util.Try(TaggedSortedStrictMap.from(xs)).toEither.left.map(_.getMessage),
      _.toSortedMap.values.toVector)

  given [A, B](using a: Schema[A], b: Schema[B]): Schema[(A, B)] = Schema.derived

  // ---- value -----------------------------------------------------------

  /** one asset of a `MultiAsset`, flat — the row an explode gives */
  final case class AssetQuantity(policy: PolicyId, name: AssetName, quantity: Long)
  given Schema[AssetName] = Schema.wrap[AssetName, Array[Byte]](b => AssetName(ByteString.fromArray(b)), _.bytes.bytes)
  given Schema[AssetQuantity] = Schema.derived

  given Schema[MultiAsset] = Schema.wrap[MultiAsset, Vector[AssetQuantity]](
    xs => MultiAsset.from(xs.map(a => (a.policy, a.name, a.quantity))),
    ma => ma.assets.toVector.flatMap((p, names) => names.toVector.map((n, q) => AssetQuantity(p, n, q))))

  given Schema[Mint] = Schema.wrap[Mint, MultiAsset](Mint(_), m => m)
  given Schema[Value] = Schema.derived

  /** a plain class (not a case class) in scalus, so no Mirror: its two numbers */
  final case class Ratio(numerator: Long, denominator: Long)
  given Schema[Ratio] = Schema.derived
  given Schema[NonNegativeInterval] = Schema.wrap[NonNegativeInterval, Ratio](
    r => NonNegativeInterval(r.numerator, r.denominator), n => Ratio(n.numerator, n.denominator))

  // ---- the recursive three, explicit (derivation needs the given for its back edge)

  given Schema[Data] = Schema.derived
  given Schema[Timelock] = Schema.derived
  given Schema[Metadatum] = Schema.derived

  // ---- the model -------------------------------------------------------

  given Schema[UnitInterval] = Schema.derived
  given Schema[ExUnits] = Schema.derived
  given Schema[ExUnitPrices] = Schema.derived
  given Schema[CostModels] = Schema.derived
  given Schema[DRepVotingThresholds] = Schema.derived
  given Schema[PoolVotingThresholds] = Schema.derived
  given Schema[ProtocolVersion] = Schema.derived
  given Schema[ProtocolParamUpdate] = Schema.derived
  given Schema[GovActionId] = Schema.derived
  given Schema[Anchor] = Schema.derived
  given Schema[Constitution] = Schema.derived
  given Schema[Relay] = Schema.derived
  given Schema[PoolMetadata] = Schema.derived
  given Schema[RewardAccount] = Schema.derived
  given Schema[DRep] = Schema.derived
  given Schema[Vote] = Schema.derived
  given Schema[Voter] = Schema.derived
  given Schema[VotingProcedure] = Schema.derived
  given Schema[VKeyWitness] = Schema.derived
  given Schema[BootstrapWitness] = Schema.derived
  given Schema[RedeemerTag] = Schema.derived
  given Schema[Redeemer] = Schema.derived
  given Schema[OperationalCert] = Schema.derived
  given Schema[VrfCert] = Schema.derived
  given Schema[BlockHeaderBody] = Schema.derived
  given Schema[TransactionInput] = Schema.derived
  given Schema[Credential] = Schema.derived
  given Schema[Script] = Schema.derived
  given Schema[ScriptRef] = Schema.derived
  given Schema[DatumOption] = Schema.derived
  given Schema[TransactionOutput] = Schema.derived
  given Schema[Certificate] = Schema.derived
  given Schema[Withdrawals] = Schema.derived
  given Schema[GovAction] = Schema.derived
  given Schema[ProposalProcedure] = Schema.derived
  given Schema[VotingProcedures] = Schema.derived
  given Schema[TransactionBody] = Schema.derived
  given Schema[Redeemers] = Schema.derived
  // the KeyOf instances for the witness set's tagged maps live in its
  // companion, outside KeyOf's implicit scope: scalus imports them too
  import TransactionWitnessSet.given
  given Schema[TransactionWitnessSet] = Schema.derived
  given Schema[AuxiliaryData] = Schema.derived
  given Schema[Transaction] = Schema.derived
  given Schema[BlockHeader] = Schema.derived
  given Schema[Block] = Schema.derived
