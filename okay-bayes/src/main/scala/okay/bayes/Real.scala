package okay.bayes

/**
 * REVERSE-MODE AUTOMATIC DIFFERENTIATION (specs/okay-bayes.md stage 3b;
 * Griewank & Walther, *Evaluating Derivatives*, 2008). A `Real` is a value
 * and, when it depends on an input, its place on a `Tape`: every operation
 * records its operands and its local partial derivatives, and one backward
 * sweep from the output gives the derivative with respect to EVERY input —
 * the cost of the function once more, whatever the number of inputs.
 *
 * A `Real` off any tape is a constant; a `Double` converts to one, so a
 * density reads `Normal(0, 10)` as well as `Normal(mu, sigma)`.
 */
into final class Real private[bayes] (val value: Double, private[bayes] val tape: Tape, private[bayes] val id: Int):
  import Real.{binary, unary}
  def +(o: Real): Real = binary(this, o, value + o.value, 1, 1)
  def -(o: Real): Real = binary(this, o, value - o.value, 1, -1)
  def *(o: Real): Real = binary(this, o, value * o.value, o.value, value)
  def /(o: Real): Real = binary(this, o, value / o.value, 1 / o.value, -value / (o.value * o.value))
  def unary_- : Real = unary(this, -value, -1)
  override def toString: String = if id < 0 then s"Real($value)" else s"Real($value, #$id)"

object Real:
  given Conversion[Double, Real] = const(_)
  given Conversion[Int, Real] = i => const(i.toDouble)

  /** constants live on no tape; this one is never recorded on */
  private val none = new Tape

  def const(x: Double): Real = new Real(x, none, -1)

  private[bayes] def binary(x: Real, y: Real, v: Double, dx: Double, dy: Double): Real =
    if x.id >= 0 then
      if y.id >= 0 then
        require(x.tape eq y.tape, "Real: operands recorded on two different tapes")
        new Real(v, x.tape, x.tape.push(x.id, dx, y.id, dy))
      else new Real(v, x.tape, x.tape.push(x.id, dx, -1, 0))
    else if y.id >= 0 then new Real(v, y.tape, y.tape.push(y.id, dy, -1, 0))
    else const(v)

  private[bayes] def unary(x: Real, v: Double, dx: Double): Real =
    if x.id >= 0 then new Real(v, x.tape, x.tape.push(x.id, dx, -1, 0)) else const(v)

  def exp(x: Real): Real = { val e = math.exp(x.value); unary(x, e, e) }
  def log(x: Real): Real = unary(x, math.log(x.value), 1 / x.value)
  def log1p(x: Real): Real = unary(x, math.log1p(x.value), 1 / (1 + x.value))
  def sqrt(x: Real): Real = { val r = math.sqrt(x.value); unary(x, r, 0.5 / r) }
  def pow(x: Real, k: Double): Real = unary(x, math.pow(x.value, k), k * math.pow(x.value, k - 1))
  def square(x: Real): Real = unary(x, x.value * x.value, 2 * x.value)
  /** log(1 + eˣ), stable on both sides */
  def softplus(x: Real): Real =
    val v = if x.value > 0 then x.value + math.log1p(math.exp(-x.value)) else math.log1p(math.exp(x.value))
    unary(x, v, 1 / (1 + math.exp(-x.value)))
  def sigmoid(x: Real): Real = { val s = 1 / (1 + math.exp(-x.value)); unary(x, s, s * (1 - s)) }
  /** ln Γ(x), its derivative the digamma function */
  def lgamma(x: Real): Real = unary(x, Distribution.logGamma(x.value), digamma(x.value))
  /** log Σ eˣⁱ — the max taken out, every term's derivative its softmax weight */
  def logSumExp(xs: Seq[Real]): Real =
    val top = xs.iterator.map(_.value).max
    if top == Double.NegativeInfinity then const(top)
    else log(sum(xs.map(x => exp(x - top)))) + top
  def sum(xs: Iterable[Real]): Real = xs.foldLeft(const(0.0))(_ + _)

  /** ψ(x): the recurrence up to 10, then the asymptotic series (its next term under 1e-15 there) */
  def digamma(x0: Double): Double =
    var x = x0
    var acc = 0.0
    while x < 10 do { acc -= 1 / x; x += 1 }
    val f = 1 / (x * x)
    acc + math.log(x) - 0.5 / x - f * (1.0 / 12 - f * (1.0 / 120 - f * (1.0 / 252 - f * (1.0 / 240 - f / 132))))


/** a number on the left of a `Real`: `0.5 * x`, `1 - p` */
extension (d: Double)
  def +(x: Real): Real = Real.const(d) + x
  def -(x: Real): Real = Real.const(d) - x
  def *(x: Real): Real = Real.const(d) * x
  def /(x: Real): Real = Real.const(d) / x

/**
 * THE TAPE: per node, up to two operands and the partial derivative with
 * respect to each. Grows by doubling; one tape per gradient evaluation.
 */
final class Tape(capacity: Int = 256):
  private var size = 0
  private var pa = new Array[Int](math.max(1, capacity))
  private var da = new Array[Double](math.max(1, capacity))
  private var pb = new Array[Int](math.max(1, capacity))
  private var db = new Array[Double](math.max(1, capacity))

  private[bayes] def push(a: Int, dA: Double, b: Int, dB: Double): Int =
    if size == pa.length then
      pa = java.util.Arrays.copyOf(pa, size * 2)
      da = java.util.Arrays.copyOf(da, size * 2)
      pb = java.util.Arrays.copyOf(pb, size * 2)
      db = java.util.Arrays.copyOf(db, size * 2)
    pa(size) = a
    da(size) = dA
    pb(size) = b
    db(size) = dB
    size += 1
    size - 1

  /** an input: a node with no operands */
  def variable(x: Double): Real = new Real(x, this, push(-1, 0, -1, 0))

  /** the number of nodes recorded */
  def length: Int = size

  /** d out / d x for each of `inputs`, by one backward sweep */
  def gradient(out: Real, inputs: IndexedSeq[Real]): Array[Double] =
    val adj = new Array[Double](size)
    if out.id >= 0 then
      require(out.tape eq this, "Tape.gradient: the output was recorded on another tape")
      adj(out.id) = 1
    var i = size - 1
    while i >= 0 do
      val g = adj(i)
      if g != 0 then
        if pa(i) >= 0 then adj(pa(i)) += g * da(i)
        if pb(i) >= 0 then adj(pb(i)) += g * db(i)
      i -= 1
    inputs.iterator.map(x => if x.id >= 0 then adj(x.id) else 0.0).toArray
