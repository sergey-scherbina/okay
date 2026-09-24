package scala2probe

import okay.codec.Schema
import okay.kafka.KafkaStore
import okay.persist.{Ack, Topic}
import okay.scala2._
import org.apache.kafka.clients.consumer.KafkaConsumer
import org.apache.kafka.clients.producer.{KafkaProducer, ProducerRecord}

object LiveModel {
  final case class Pet(id: Long, name: String)
  object Pet {
    implicit val schema: Schema[Pet] = Schemas.product2("Pet", "id", "name")(Pet.apply)(p => (p.id, p.name))
  }
}

/** okay-pg and okay-kafka from Scala 2.13, against real servers: Live, out of
 * the default gate (`sbt integrationTest`); each test skips when its server is
 * absent (OKAY_PG_HOST/OKAY_PG_PORT, OKAY_KAFKA, as okay-pg's and okay-kafka's
 * own suites) */
class TestServicesLiveFromScala2 extends munit.FunSuite {
  import LiveModel._

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))

  val pgHost = sys.env.getOrElse("OKAY_PG_HOST", "127.0.0.1")
  val pgPort = sys.env.get("OKAY_PG_PORT").flatMap(p => scala.util.Try(p.toInt).toOption).getOrElse(5432)
  val bootstrap = sys.env.getOrElse("OKAY_KAFKA", "127.0.0.1:9092")

  def reachable(host: String, port: Int): Boolean =
    try { val s = new java.net.Socket(); s.connect(new java.net.InetSocketAddress(host, port), 1000); s.close(); true }
    catch { case _: java.io.IOException => false }

  test("Postgres over okay-pg's own wire protocol, as the SQL facade's Db") {
    assume(reachable(pgHost, pgPort), s"no Postgres at $pgHost:$pgPort — the live suite skips")
    val table = "s2pets_" + System.nanoTime
    val prog = for {
      db <- Postgres.connect(pgHost, pgPort, "okay", "okay", "okay")
      _ <- db.update(s"create table $table (id bigint primary key, name text not null)")
      _ <- db.updateOf(s"insert into $table (id, name) values ($$1, $$2)", Pet(1, "Rex"))
      pets <- Throws.runEither(db.all[Pet](s"select id, name from $table"))
      _ <- db.update(s"drop table $table")
    } yield pets
    assertEquals(prog.runWith, Right(Vector(Pet(1, "Rex"))))
  }

  test("Kafka as an okay-persist store, and a producer and a consumer through Kafkas") {
    val (host, port) = bootstrap.splitAt(bootstrap.lastIndexOf(':'))
    assume(reachable(host, port.drop(1).toInt), s"no Kafka at $bootstrap — the live suite skips")
    val name = "s2-" + System.nanoTime
    val store = new KafkaStore(bootstrap)
    try {
      val t = Persist.topic(store, name)
      t.append("k".getBytes, "via-store".getBytes, Ack.Durable)
      t.read(0, 0L, 10) match {
        case Topic.Read.Records(rs) => assertEquals(rs.map(r => new String(r.value)), Vector("via-store"))
        case other => fail(other.toString)
      }
    } finally store.close()

    val props = new java.util.Properties()
    props.put("bootstrap.servers", bootstrap)
    props.put("key.serializer", "org.apache.kafka.common.serialization.StringSerializer")
    props.put("value.serializer", "org.apache.kafka.common.serialization.StringSerializer")
    val producer = new KafkaProducer[String, String](props)
    Kafkas.send(producer, Seq(new ProducerRecord[String, String](name + "-raw", "k", "hello"))).runWith
    producer.close()

    val cprops = new java.util.Properties()
    cprops.put("bootstrap.servers", bootstrap)
    cprops.put("group.id", name)
    cprops.put("auto.offset.reset", "earliest")
    cprops.put("enable.auto.commit", "false")
    cprops.put("key.deserializer", "org.apache.kafka.common.serialization.StringDeserializer")
    cprops.put("value.deserializer", "org.apache.kafka.common.serialization.StringDeserializer")
    val consumer = new KafkaConsumer[String, String](cprops)
    consumer.subscribe(java.util.List.of(name + "-raw"))
    try {
      val got = Kafkas.source(consumer, 200).take(1).map(_.value).runCollect.runWith
      assertEquals(got, Vector("hello"))
      Kafkas.commit(consumer).runWith
    } finally consumer.close()
  }
}
