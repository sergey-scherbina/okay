package okay.scalus

import okay.chain.*
import scalus.cardano.ledger.{Block, BlockFile, OriginalCborByteArray, Transaction}
import scalus.uplc.builtin.{ByteString, platform}

/**
 * What chain-sync says about a block before its body is fetched: read
 * from the header's own bytes (the first three fields of a header body
 * are the same in every Shelley-or-later era: block number, slot,
 * previous hash), and the block's identity is Blake2b-256 of those
 * bytes. NOT scalus's `Block.hash`, which returns the BODY hash
 * (Block.scala:50) — the trap specs/scalus.md §1 records.
 */
final case class Header(era: Int, blockNo: Long, slot: Long, hash: String, prev: Option[String], bytes: Array[Byte]):
  def point: N2N.Pt = N2N.Pt(slot, Header.unhex(hash))

object Header:
  def hex(b: Array[Byte]): String = b.map(x => f"${x & 0xFF}%02x").mkString
  def unhex(s: String): Array[Byte] = s.grouped(2).map(Integer.parseInt(_, 16).toByte).toArray
  def blake2b256(b: Array[Byte]): Array[Byte] = platform.blake2b_256(ByteString.fromArray(b)).bytes

  /** a header as a block to the follower: its number, its hash, its
   * parent — confirming needs nothing else, and it has no transactions */
  given BlockOf.Aux[Header, Nothing] = new BlockOf[Header]:
    type Tx = Nothing
    def ref(h: Header): BlockRef = BlockRef(Point(h.blockNo, BlockId(h.hash)), BlockId(h.prev.getOrElse("")), None)
    def txs(h: Header): Vector[Nothing] = Vector.empty

  def parse(era: Int, bytes: Array[Byte]): Either[String, Header] =
    Cv.read(bytes) match
      case Cv.Read.Done(Cv.Arr(Cv.Arr(body) +: _), _) => body.take(3) match
        case Vector(Cv.UInt(no), Cv.UInt(slot), prev) =>
          val p = prev match
            case Cv.Bytes(h) => Right(Some(hex(h)))
            case Cv.Null => Right(None)
            case other => Left(s"not a previous hash: $other")
          p.map(Header(era, no.toLong, slot.toLong, hex(blake2b256(bytes)), _, bytes))
        case other => Left(s"not a header body: $other")
      case other => Left(s"not a header: $other")

/**
 * A block as it came off the wire: the header chain-sync announced, and
 * the body block-fetch returned as `[era, block]` bytes. Decoding the
 * body is scalus's, on demand — a consumer that wants only heights and
 * hashes never pays for it.
 */
final case class CardanoBlock(header: Header, bytes: Array[Byte], time: Option[Long] = None):
  lazy val file: BlockFile = BlockFile.fromCborArray(bytes)
  def block: Block = file.block
  def transactions: Seq[Transaction] =
    given OriginalCborByteArray = OriginalCborByteArray(bytes)
    file.block.transactions

object CardanoBlock:
  /** the follower's view: a block's height is its block NUMBER, its id
   * the header hash, its time the slot's (epoch millis), when the
   * source knew the network's slot config */
  given BlockOf.Aux[CardanoBlock, Transaction] = new BlockOf[CardanoBlock]:
    type Tx = Transaction
    def ref(b: CardanoBlock): BlockRef =
      BlockRef(Point(b.header.blockNo, BlockId(b.header.hash)),
        BlockId(b.header.prev.getOrElse("")), b.time)
    def txs(b: CardanoBlock): Vector[Transaction] = b.transactions.toVector
