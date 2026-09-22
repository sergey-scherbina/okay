package okay.chain

/** a toy block for the follower's tests: height, id, parent */
final case class B(height: Long, id: String, parent: String)

object B:
  given BlockOf[B] with
    type Tx = Nothing
    def ref(b: B): BlockRef = BlockRef(Point(b.height, BlockId(b.id)), BlockId(b.parent), None)
    def txs(b: B): Vector[Nothing] = Vector.empty

  def p(height: Long, id: String): Point = Point(height, BlockId(id))

  /** a linked run `fork{from}..fork{to}`, the first extending `parent` */
  def run(from: Long, to: Long, fork: String, parent: String): Vector[B] =
    (from to to).foldLeft(Vector.empty[B]) { (acc, h) =>
      acc :+ B(h, s"$fork$h", acc.lastOption.fold(parent)(_.id))
    }
