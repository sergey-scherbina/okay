package okay.chain

import okay.codec.{Json, Schema}

class TestIds extends munit.FunSuite:

  test("CAIP-2: the registry's and x402's examples parse and render back") {
    for s <- List("eip155:8453", "eip155:84532", "solana:5eykt4UsFv8P8NJdTREpY1vzqKqZKvdp",
                  "bip122:000000000019d6689c085ae165831e93", "tron:728126428", "cip34:1-764824073") do
      assertEquals(Network.parse(s).map(_.toString), Right(s))
  }

  test("CAIP-2 refuses what its syntax refuses") {
    for s <- List("", "eip155", "EIP155:1", "ab:1", "toolongns:1", "eip155:", "eip155:a/b") do
      assert(Network.parse(s).isLeft, s)
  }

  test("CAIP-10 and CAIP-19 parse, render back, and name their network") {
    val a = "eip155:8453:0x209693Bc6afc0C5328bA36FaF03C514EF312287C"
    assertEquals(Account.parse(a).map(_.toString), Right(a))
    assertEquals(Account.parse(a).map(_.network), Right(Network.base))
    val usdc = "eip155:8453/erc20:0x833589fcd6edb6e08f4c7c32d4f71b54bda02913"
    assertEquals(Asset.parse(usdc).map(_.toString), Right(usdc))
    assertEquals(Asset.native(Network.cardano, 1815).toString, "cip34:1-764824073/slip44:1815")
    assert(Account.parse("eip155:8453").isLeft)
    assert(Asset.parse("eip155:8453/ERC20:0x1").isLeft)
  }

  final case class Req(network: Network, amount: Amount, asset: Asset, payTo: Account)
  given Schema[Req] = Schema.derived

  test("on a JSON wire the ids are their CAIP strings and an amount is a digit string — x402's shape") {
    val r = Req(Network.base, BigInt("10000"),
      Asset(Network.base, "erc20", "0x036CbD53842c5426634e7929541eC2318f3dCF7e"),
      Account(Network.base, "0x209693Bc6afc0C5328bA36FaF03C514EF312287C"))
    val text = Json.write(r)
    assert(text.contains("\"network\":\"eip155:8453\""), text)
    assert(text.contains("\"amount\":\"10000\""), text)
    assertEquals(Json.read[Req](text), Right(r))
    assert(Json.read[Req](text.replace("eip155:8453\"", "nope\"")).isLeft)
  }
