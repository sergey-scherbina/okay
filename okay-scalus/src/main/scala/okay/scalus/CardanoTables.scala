package okay.scalus

import okay.codec.Schema
import CardanoSchemas.given
import scalus.cardano.ledger.{Certificate, DatumOption, RedeemerTag, Script, TransactionOutput}
import scalus.uplc.builtin.Data

/**
 * A block as TABLES — typed rows, no engine (specs/scalus.md §5, as
 * decided 2026-09-23): the explode from one block to its transactions,
 * inputs, outputs, assets, mints, certificates, withdrawals and
 * redeemers is written ONCE, here, and every consumer reads the same
 * rows — okay-watch's own rules and aggregators, `Columns` into DuckDB
 * or Delta, and Spark (okay-scalus-spark derives its DataFrames from
 * these types; it adds no logic).
 *
 * Every row carries where it came from — `slot`, `blockNo`,
 * `blockHash`, `time` — so any table joins back to its block and any
 * table can be windowed by event time. Ids are the chain's canonical
 * hex strings, as okay-chain's `BlockId`/`TxId` are.
 *
 * Sums stay sums: a certificate is scalus's `Certificate`, a datum
 * scalus's `DatumOption`; `Columns` gives them their tabular shape
 * (`kind` + branches; `Data` as cbor + json).
 *
 * What a block alone cannot say is not guessed: an input is a REFERENCE
 * `(spentTx, spentIndex)` — its address and value are the output it
 * names, so resolving it is `outputs` joined to `inputs` by those two
 * columns.
 */
object CardanoTables:

  final case class BlockRow(slot: Long, blockNo: Long, blockHash: String, prevHash: Option[String],
                            time: Option[Long], era: Int, txCount: Int, invalidTxCount: Int) derives Schema

  /** `cbor` is the transaction body's exact bytes: `txHash` is their
   * Blake2b-256, so they are kept (specs/scalus.md §4.3) */
  final case class TransactionRow(slot: Long, blockNo: Long, blockHash: String, time: Option[Long],
                                  txIndex: Int, txHash: String, valid: Boolean, fee: Long,
                                  inputs: Int, outputs: Int, ttl: Option[Long], validFrom: Option[Long],
                                  cbor: Array[Byte]) derives Schema

  /** which list of the body an input is in, and whether the chain spent it */
  enum InputRole derives Schema:
    case Spend, Collateral, Reference

  /**
   * `spent` is the LEDGER's answer, not the list's: a valid transaction
   * spends its inputs; one whose scripts failed (`valid = false`) spends
   * its collateral instead; a reference input is never spent.
   */
  final case class InputRow(slot: Long, blockNo: Long, blockHash: String, time: Option[Long],
                            txHash: String, role: InputRole, position: Int,
                            spentTx: String, spentIndex: Int, spent: Boolean) derives Schema

  /** an output the chain CREATED: a valid transaction's outputs, or an
   * invalid one's collateral return (at index = number of outputs) */
  final case class OutputRow(slot: Long, blockNo: Long, blockHash: String, time: Option[Long],
                             txHash: String, index: Int, address: String, lovelace: Long,
                             assetCount: Int, datum: Option[DatumOption], scriptRef: Option[Script],
                             collateralReturn: Boolean) derives Schema

  /** one native asset held by one output — the explode of `value.assets` */
  final case class AssetRow(slot: Long, blockNo: Long, blockHash: String, time: Option[Long],
                            txHash: String, index: Int, policy: String, name: String, quantity: Long) derives Schema

  /** minted (positive) or burned (negative) */
  final case class MintRow(slot: Long, blockNo: Long, blockHash: String, time: Option[Long],
                           txHash: String, policy: String, name: String, quantity: Long) derives Schema

  final case class CertificateRow(slot: Long, blockNo: Long, blockHash: String, time: Option[Long],
                                  txHash: String, index: Int, cert: Certificate) derives Schema

  final case class WithdrawalRow(slot: Long, blockNo: Long, blockHash: String, time: Option[Long],
                                 txHash: String, rewardAccount: String, lovelace: Long) derives Schema

  final case class RedeemerRow(slot: Long, blockNo: Long, blockHash: String, time: Option[Long],
                               txHash: String, tag: RedeemerTag, index: Int, data: Data,
                               memory: Long, steps: Long) derives Schema

  /** every table of one block */
  final case class Tables(blocks: Vector[BlockRow], transactions: Vector[TransactionRow],
                          inputs: Vector[InputRow], outputs: Vector[OutputRow], assets: Vector[AssetRow],
                          mints: Vector[MintRow], certificates: Vector[CertificateRow],
                          withdrawals: Vector[WithdrawalRow], redeemers: Vector[RedeemerRow]):
    def ++(o: Tables): Tables = Tables(blocks ++ o.blocks, transactions ++ o.transactions,
      inputs ++ o.inputs, outputs ++ o.outputs, assets ++ o.assets, mints ++ o.mints,
      certificates ++ o.certificates, withdrawals ++ o.withdrawals, redeemers ++ o.redeemers)

  object Tables:
    val empty: Tables = Tables(Vector.empty, Vector.empty, Vector.empty, Vector.empty, Vector.empty,
      Vector.empty, Vector.empty, Vector.empty, Vector.empty)

  private def address(o: TransactionOutput): String =
    o.address.encode.getOrElse(Header.hex(o.address.toBytes.bytes))

  /** the whole explode of one block */
  def of(b: CardanoBlock): Tables =
    val h = b.header
    val (slot, no, bh, t) = (h.slot, h.blockNo, h.hash, b.time)
    val txs = b.transactions.toVector
    val invalid = b.block.invalidTransactions.toSet
    val tables = txs.zipWithIndex.map { (tx, i) =>
      val body = tx.body.value
      val valid = !invalid(i)
      val id = tx.id.toHex

      def refs(role: InputRole, xs: Seq[scalus.cardano.ledger.TransactionInput], spent: Boolean) =
        xs.zipWithIndex.map((in, p) =>
          InputRow(slot, no, bh, t, id, role, p, in.transactionId.toHex, in.index, spent)).toVector

      val created: Vector[(Int, TransactionOutput, Boolean)] =
        if valid then body.outputs.toVector.zipWithIndex.map((s, j) => (j, s.value, false))
        else body.collateralReturnOutput.map(s => (body.outputs.size, s.value, true)).toVector

      val outputs = created.map((j, o, ret) =>
        OutputRow(slot, no, bh, t, id, j, address(o), o.value.coin.value, o.value.assets.assets.valuesIterator.map(_.size).sum,
          o.datumOption, o.scriptRef.map(_.script), ret))

      val assets = created.flatMap((j, o, _) =>
        o.value.assets.assets.toVector.flatMap((policy, names) =>
          names.toVector.map((name, q) => AssetRow(slot, no, bh, t, id, j, policy.toHex, name.bytes.toHex, q))))

      val mints = body.mint.toVector.flatMap(m =>
        m.assets.toVector.flatMap((policy, names) =>
          names.toVector.map((name, q) => MintRow(slot, no, bh, t, id, policy.toHex, name.bytes.toHex, q))))

      val certs = body.certificates.toSeq.toVector.zipWithIndex.map((c, k) => CertificateRow(slot, no, bh, t, id, k, c))

      val withdrawals = body.withdrawals.toVector.flatMap(w =>
        w.withdrawals.toVector.map((acct, coin) =>
          WithdrawalRow(slot, no, bh, t, id, acct.address.encode.getOrElse(Header.hex(acct.address.toBytes.bytes)), coin.value)))

      val redeemers = tx.witnessSet.redeemers.toVector.flatMap(_.value.toSeq).map(r =>
        RedeemerRow(slot, no, bh, t, id, r.tag, r.index, r.data, r.exUnits.memory, r.exUnits.steps))

      Tables(Vector.empty,
        Vector(TransactionRow(slot, no, bh, t, i, id, valid, body.fee.value,
          body.inputs.toSeq.size, body.outputs.size, body.ttl, body.validityStartSlot, tx.body.raw)),
        refs(InputRole.Spend, body.inputs.toSeq, valid) ++
          refs(InputRole.Collateral, body.collateralInputs.toSeq, !valid) ++
          refs(InputRole.Reference, body.referenceInputs.toSeq, false),
        outputs, assets, mints, certs, withdrawals, redeemers)
    }.foldLeft(Tables.empty)(_ ++ _)
    tables.copy(blocks = Vector(BlockRow(slot, no, bh, h.prev, t, b.file.era, txs.size, invalid.size)))

  /** one table: its name, how a block's `Tables` yield its rows, and its
   * `Schema` — what an engine adapter (Spark, Flink) needs to serve it by
   * name without knowing the row types */
  final class Table[A](val name: String, val pick: Tables => Vector[A])(using val schema: Schema[A])

  /** every table, by the name an adapter's `table` option uses */
  val all: Vector[Table[?]] = Vector(
    Table[BlockRow]("blocks", _.blocks),
    Table[TransactionRow]("transactions", _.transactions),
    Table[InputRow]("inputs", _.inputs),
    Table[OutputRow]("outputs", _.outputs),
    Table[AssetRow]("assets", _.assets),
    Table[MintRow]("mints", _.mints),
    Table[CertificateRow]("certificates", _.certificates),
    Table[WithdrawalRow]("withdrawals", _.withdrawals),
    Table[RedeemerRow]("redeemers", _.redeemers))

  def named(name: String): Table[?] =
    all.find(_.name == name).getOrElse(throw IllegalArgumentException(
      s"unknown table '$name'; one of: ${all.map(_.name).mkString(", ")}"))
