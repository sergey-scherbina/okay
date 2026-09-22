package okay.chain

import okay.codec.Schema

/**
 * A network, CAIP-2 (specs/chain.md §1): `namespace:reference` —
 * `eip155:8453`, `solana:5eykt4UsFv8P8NJdTREpY1vzqKqZKvdp`,
 * `bip122:000000000019d6689c085ae165831e93`. x402 v2 names networks
 * exactly so, which is why this module does. Cardano has no registered
 * namespace (the ChainAgnostic registry, checked 2026-09-23 at
 * 463bae5, has neither `cardano` nor `cip34`); `Network.cardano` uses
 * CIP-34's own `cip34:{networkId}-{networkMagic}`, which the CAIP-2
 * syntax admits.
 */
final case class Network(namespace: String, reference: String):
  override def toString: String = s"$namespace:$reference"

object Network:
  // CAIP-2: namespace [-a-z0-9]{3,8}, reference [-_a-zA-Z0-9]{1,32}
  private val Caip2 = """([-a-z0-9]{3,8}):([-_a-zA-Z0-9]{1,32})""".r

  def parse(s: String): Either[String, Network] = s match
    case Caip2(ns, ref) => Right(Network(ns, ref))
    case _ => Left(s"not a CAIP-2 network id: '$s'")

  val cardano: Network = Network("cip34", "1-764824073")
  val cardanoPreprod: Network = Network("cip34", "0-1")
  val cardanoPreview: Network = Network("cip34", "0-2")
  val bitcoin: Network = Network("bip122", "000000000019d6689c085ae165831e93")
  val ethereum: Network = Network("eip155", "1")
  val base: Network = Network("eip155", "8453")
  val solana: Network = Network("solana", "5eykt4UsFv8P8NJdTREpY1vzqKqZKvdp")

  given Schema[Network] = Schema.refine[Network, String](parse, _.toString)

/** an account on a network, CAIP-10: `network:address`, the address in
 * the chain's canonical rendering (bech32, EIP-55 hex, base58) */
final case class Account(network: Network, address: String):
  override def toString: String = s"$network:$address"

object Account:
  // CAIP-10 account_address: [-.%a-zA-Z0-9]{1,128}
  private val Addr = """[-.%a-zA-Z0-9]{1,128}""".r

  def parse(s: String): Either[String, Account] =
    val i = s.lastIndexOf(':')
    if i < 0 then Left(s"not a CAIP-10 account id: '$s'")
    else
      val addr = s.substring(i + 1)
      if !Addr.matches(addr) then Left(s"not a CAIP-10 account address: '$addr'")
      else Network.parse(s.substring(0, i)).map(Account(_, addr))

  given Schema[Account] = Schema.refine[Account, String](parse, _.toString)

/**
 * An asset, CAIP-19: `network/namespace:reference` —
 * `eip155:8453/erc20:0x833589fcd6edb6e08f4c7c32d4f71b54bda02913`, the
 * native coin as `slip44:<coin type>` (`eip155:1/slip44:60`,
 * `cip34:1-764824073/slip44:1815`).
 */
final case class Asset(network: Network, namespace: String, reference: String):
  override def toString: String = s"$network/$namespace:$reference"

object Asset:
  // CAIP-19 asset_namespace [-a-z0-9]{3,8}, asset_reference [-.%a-zA-Z0-9]{1,128}
  private val Caip19 = """(.+)/([-a-z0-9]{3,8}):([-.%a-zA-Z0-9]{1,128})""".r

  def parse(s: String): Either[String, Asset] = s match
    case Caip19(net, ns, ref) => Network.parse(net).map(Asset(_, ns, ref))
    case _ => Left(s"not a CAIP-19 asset id: '$s'")

  /** a chain's native coin, by its SLIP-44 coin type */
  def native(network: Network, slip44: Int): Asset = Asset(network, "slip44", slip44.toString)

  given Schema[Asset] = Schema.refine[Asset, String](parse, _.toString)

/** a block's identity in the chain's canonical rendering (hex, base58)
 * — a string, not bytes, so a source adopts it with no conversion
 * (specs/chain.md §1) */
opaque type BlockId = String
object BlockId:
  def apply(s: String): BlockId = s
  extension (b: BlockId) def value: String = b
  given Schema[BlockId] = Schema.wrap[BlockId, String](apply, _.value)

/** a transaction's identity, as `BlockId` */
opaque type TxId = String
object TxId:
  def apply(s: String): TxId = s
  extension (t: TxId) def value: String = t
  given Schema[TxId] = Schema.wrap[TxId, String](apply, _.value)

/** an amount in ATOMIC units (lovelace, wei, satoshi): EVM amounts are
 * uint256, so nothing narrower holds them; on a JSON wire it is a
 * digit string (schema-bigint), which is x402's own `amount` */
type Amount = BigInt
