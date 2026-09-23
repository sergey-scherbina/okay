package okay.scalus

import okay.chain.*
import scalus.cardano.ledger.Transaction

/**
 * Cardano's transactions through okay-chain's `Ledger` (specs/chain.md
 * §4). What a block alone can and cannot say:
 *
 * - `movements`: one per asset per output, `to` the output's address,
 *   and `from = None` — an input is a REFERENCE to an earlier output,
 *   so who paid is known only with the UTXO set (in Spark, `outputs`
 *   joined to `inputs`); an attribution invented here would be a guess.
 * - `utxo`: the lossless view, exact.
 * - ADA is `slip44:1815`; a native token is `token:<policy>.<name>`
 *   (hex), okay's own convention — CAIP-19 registers no Cardano asset
 *   namespace (specs/chain.md §1).
 */
final class CardanoLedger(val network: Network) extends Ledger[Transaction]:
  val ada: Asset = Asset.native(network, 1815)

  def token(policy: String, name: String): Asset = Asset(network, "token", s"$policy.$name")

  def id(tx: Transaction): TxId = TxId(tx.id.toHex)
  def fee(tx: Transaction): Option[Amount] = Some(BigInt(tx.body.value.fee.value))

  private def address(o: scalus.cardano.ledger.TransactionOutput): Account =
    Account(network, o.address.encode.getOrElse(Header.hex(o.address.toBytes.bytes)))

  private def holds(o: scalus.cardano.ledger.TransactionOutput): Vector[(Asset, Amount)] =
    val v = o.value
    (ada -> BigInt(v.coin.value)) +: v.assets.assets.toVector.flatMap((policy, names) =>
      names.toVector.map((name, q) => token(policy.toHex, name.bytes.toHex) -> BigInt(q)))

  def movements(tx: Transaction): Vector[Movement] =
    tx.body.value.outputs.toVector.flatMap { sized =>
      val o = sized.value
      holds(o).map((asset, q) => Movement(asset, q, None, Some(address(o))))
    }

  override def utxo(tx: Transaction): Option[UtxoView] =
    val me = id(tx)
    Some(UtxoView(
      tx.body.value.inputs.toSeq.toVector.map(i => OutRef(TxId(i.transactionId.toHex), i.index)),
      tx.body.value.outputs.toVector.zipWithIndex.map((s, i) => Output(OutRef(me, i), address(s.value), holds(s.value)))))
