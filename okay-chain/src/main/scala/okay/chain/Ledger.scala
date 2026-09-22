package okay.chain

import okay.codec.Schema

/**
 * Value that moved in a transaction, as a uniform PROJECTION of it
 * (specs/chain.md §4) — beside the native transaction, never instead
 * of it.
 *
 * - `from`/`to` are optional: a mint has no `from`, a burn no `to`;
 * - on a UTXO chain `from` is an ATTRIBUTION (which input paid); the
 *   truth is `Ledger.utxo`;
 * - `complete = false` is how a source SAYS it saw only part of what
 *   moved — an EVM source reading logs cannot see a contract's
 *   internal ETH transfers without a trace API — so a caller learns
 *   it from the value, not from a wrong total.
 */
final case class Movement(asset: Asset, amount: Amount,
                          from: Option[Account], to: Option[Account],
                          complete: Boolean = true) derives Schema

/** a transaction output, by the transaction that created it */
final case class OutRef(tx: TxId, index: Int) derives Schema

/** an output created: where it sits and what it holds */
final case class Output(ref: OutRef, owner: Account, holds: Vector[(Asset, Amount)]) derives Schema

/** the lossless view of a UTXO transaction: what it spent, what it made */
final case class UtxoView(spent: Vector[OutRef], created: Vector[Output]) derives Schema

/** the uniform read of a native transaction `Tx` */
trait Ledger[Tx]:
  def network: Network
  def id(tx: Tx): TxId
  def fee(tx: Tx): Option[Amount]
  def movements(tx: Tx): Vector[Movement]
  /** UTXO chains only; an account chain answers None */
  def utxo(tx: Tx): Option[UtxoView] = None
