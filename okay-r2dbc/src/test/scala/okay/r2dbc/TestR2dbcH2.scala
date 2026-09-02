package okay.r2dbc

import io.r2dbc.h2.{H2ConnectionConfiguration, H2ConnectionFactory}
import io.r2dbc.spi.Connection

/** H2 through r2dbc-h2: the in-matrix engine, no server needed */
class TestR2dbcH2 extends R2dbcSuite:
  def engine = "h2"
  private val factory = H2ConnectionFactory(H2ConnectionConfiguration.builder()
    .inMemory("okay-r2dbc").option("DB_CLOSE_DELAY=-1").build())
  def open(): Connection = Rx.first(factory.create()).get
