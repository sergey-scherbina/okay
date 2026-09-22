package okay.chain

/** the projection over two toy ledgers: one UTXO, one account */
class TestLedger extends munit.FunSuite:

  private val ada = Asset.native(Network.cardano, 1815)
  private def acct(a: String) = Account(Network.cardano, a)

  /** a toy UTXO transaction: inputs by reference, outputs with owners */
  final case class UTx(id: String, inputs: Vector[(OutRef, Account, BigInt)], outputs: Vector[(Account, BigInt)], fee: BigInt)

  given Ledger[UTx] with
    def network = Network.cardano
    def id(tx: UTx) = TxId(tx.id)
    def fee(tx: UTx) = Some(tx.fee)
    /** the largest input is named payer — an attribution, as okay-watch does */
    def movements(tx: UTx) =
      val payer = tx.inputs.maxByOption(_._3).map(_._2)
      tx.outputs.filterNot((owner, _) => payer.contains(owner))
        .map((owner, q) => Movement(ada, q, payer, Some(owner)))
    override def utxo(tx: UTx) = Some(UtxoView(tx.inputs.map(_._1),
      tx.outputs.zipWithIndex.map { case ((owner, q), i) => Output(OutRef(TxId(tx.id), i), owner, Vector(ada -> q)) }))

  test("a UTXO transaction: the movement attributes a payer, the utxo view keeps every input") {
    val tx = UTx("t1",
      Vector((OutRef(TxId("t0"), 0), acct("alice"), BigInt(7_000_000)), (OutRef(TxId("t0"), 1), acct("bob"), BigInt(2_000_000))),
      Vector((acct("carol"), BigInt(5_000_000)), (acct("alice"), BigInt(3_800_000))), BigInt(200_000))
    val l = summon[Ledger[UTx]]
    assertEquals(l.movements(tx), Vector(Movement(ada, BigInt(5_000_000), Some(acct("alice")), Some(acct("carol")))))
    val u = l.utxo(tx).get
    assertEquals(u.spent.size, 2)     // bob paid too; only the utxo view says so
    assertEquals(u.created.map(_.owner), Vector(acct("carol"), acct("alice")))
  }

  /** a toy account-chain call that moved a token and, internally, ETH it could not see */
  final case class ETx(hash: String, from: String, token: String, to: String, value: BigInt, traced: Boolean)

  given Ledger[ETx] with
    def network = Network.base
    def id(tx: ETx) = TxId(tx.hash)
    def fee(tx: ETx) = None
    def movements(tx: ETx) = Vector(Movement(Asset(Network.base, "erc20", tx.token), tx.value,
      Some(Account(Network.base, tx.from)), Some(Account(Network.base, tx.to)), complete = tx.traced))

  test("an account transaction read from logs SAYS it is incomplete; a uint256 amount survives") {
    val big = (BigInt(1) << 255) + 1
    val m = summon[Ledger[ETx]].movements(ETx("0xab", "0x01", "0xcc", "0x02", big, traced = false)).head
    assert(!m.complete)
    assertEquals(m.amount, big)
    assertEquals(summon[Ledger[ETx]].utxo(ETx("0xab", "0x01", "0xcc", "0x02", big, true)), None)
  }
