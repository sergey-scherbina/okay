package okay.scalus

import okay.chain.*

/**
 * The snippet in docs/modules/okay-scalus.md, VERBATIM. It opens a
 * socket to a public relay, so the default gate COMPILES it (the API it
 * shows exists, with these types) and `TestLive` runs the same calls.
 */
class TestDocExamplesScalus extends munit.FunSuite:

  def snippet(): Unit =
    // ---- snippet begins
    CardanoFollower.open(Wire.tcp("preprod-node.play.dev.cardano.org", 3001),
                         CardanoNetwork.preprod, from = None, finality = Finality.Depth(15)) match
      case Left(why) => println(why)
      case Right(f) =>
        try
          for _ <- 1 to 3 do
            f.step().foreach(_.foreach {
              case Event.Confirmed(b) =>
                println(s"${b.header.blockNo} ${b.header.hash} ${b.transactions.size} txs")
              case Event.RolledBack(to, _) =>
                println(s"everything after block ${to.height} is void")
            })
        finally f.close()
    // ---- snippet ends

  test("docs: the follower snippet compiles against the real API") {
    val _ = () => snippet()
  }
