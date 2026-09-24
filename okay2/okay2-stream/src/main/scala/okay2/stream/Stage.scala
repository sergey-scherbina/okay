package okay2.stream

import okay2._

/**
 * A pipeline stage: a transducer as a program — it awaits I and tells
 * O, state is just its recursion parameters. Tokenizers, parsers,
 * codec dialects, any stream rewriter share this one shape;
 * composition (`Pipe.through`) is demand-driven coroutine pairing, so
 * every stage is incremental, resumable and lazy by construction.
 */
object Stage {

  /** the next input, or None — upstream ended */
  def await[I, O]: Option[I] ! (Take[I] + Writer[O]) = Take.await[I].plus[Writer[O]]

  /** emit one output — it answers nothing, like every tell */
  def tell[I, O](o: O): Unit ! (Take[I] + Writer[O]) = Writer.tell(o).at[Take[I] + Writer[O]]

  /** the identity stage: every input becomes an output */
  def id[T]: Stage[T, T, Unit] =
    await[T, T].flatMap {
      case Some(t) => tell[T, T](t).flatMap(_ => id[T])
      case None => pure(())
    }

  /**
   * The transducer skeleton, named: carry a state, step it with each
   * input (telling whatever that input is worth), and flush at the end
   * of the input. The step ANSWERS the new state and is itself a
   * stage, so it may tell nothing, one, or many outputs. `step` and
   * `end` share ONE parameter list so that the expected result type
   * reaches both lambdas' parameter types.
   */
  def transduce[I, O, S](z: S)(step: (S, I) => Stage[I, O, S], end: S => Stage[I, O, S]): Stage[I, O, S] = {
    def go(s: S): Stage[I, O, S] = await[I, O].flatMap {
      case Some(i) => step(s, i).flatMap(go)
      case None => end(s)
    }
    go(z)
  }

  /** `transduce` whose STEP may end the stage: `Left(s)` carries on,
   * `Right(r)` answers and the stage ends there, so `through` pulls
   * nothing more from upstream; `end` is the answer when the input
   * ends first */
  def transduceUntil[I, O, S, R](z: S)(step: (S, I) => Stage[I, O, Either[S, R]], end: S => R): Stage[I, O, R] = {
    def go(s: S): Stage[I, O, R] = await[I, O].flatMap {
      case Some(i) => step(s, i).flatMap {
        case Left(next) => go(next)
        case Right(r) => pure(r)
      }
      case None => pure(end(s))
    }
    go(z)
  }

  /** the stateful 1:1 map — fs2's `mapAccumulate`: exactly one output
   * per input, nothing to flush */
  def mapAccumulate[I, O, S](z: S)(f: (S, I) => (S, O)): Stage[I, O, S] =
    transduce[I, O, S](z)((s, i) => {
      val (s2, o) = f(s, i)
      tell[I, O](o).map(_ => s2)
    }, s => pure(s))

  /**
   * The PHASED transducer: a stream with phases — a header before
   * rows — where the accumulator CHANGES TYPE at the switch instead of
   * encoding the phase as a sum in S. `head` runs at S1 and either
   * stays (Left) or switches (Right) carrying the S2 the body starts
   * from; `body` runs at S2 and cannot mention S1 — by type. The
   * per-input transition is EXECUTED through `PState`, the
   * type-changing state. Ends are honest both ways: the answer says
   * which phase the stream died in.
   */
  def phased[I, O, S1, S2](z: S1)(
      head: (S1, I) => Either[(S1, Vector[O]), (S2, Vector[O])],
      body: (S2, I) => (S2, Vector[O]),
      endHead: S1 => Vector[O],
      endBody: S2 => Vector[O]): Stage[I, O, Either[S1, S2]] = {

    def tellAll(os: Vector[O]): Stage[I, O, Unit] =
      os.foldLeft(pure[Take[I] + Writer[O], Unit](()))((p, o) => p.flatMap(_ => tell[I, O](o)))

    type R = (Either[S1, S2], Vector[O])
    def switch(s1: S1, i: I): R =
      PState.run[S1, Either[S1, S2], Vector[O]](s1) {
        PState.get[S1, R].flatMap { s =>
          head(s, i) match {
            case Left((ns, os)) => PState.set[S1, Either[S1, S2], R](Left(ns)).map(_ => os)
            case Right((s2, os)) => PState.set[S1, Either[S1, S2], R](Right(s2)).map(_ => os)
          }
        }
      }

    def inHead(s1: S1): Stage[I, O, Either[S1, S2]] = await[I, O].flatMap {
      case None => tellAll(endHead(s1)).map(_ => Left(s1))
      case Some(i) =>
        val (next, os) = switch(s1, i)
        tellAll(os).flatMap { _ =>
          next match {
            case Left(ns) => inHead(ns)
            case Right(s2) => inBody(s2)
          }
        }
    }

    def inBody(s2: S2): Stage[I, O, Either[S1, S2]] = await[I, O].flatMap {
      case None => tellAll(endBody(s2)).map(_ => Right(s2))
      case Some(i) =>
        val (ns, os) = body(s2, i)
        tellAll(os).flatMap(_ => inBody(ns))
    }

    inHead(z)
  }

  /** batch inputs into chunks of the given size (the tail flushes on
   * end of input). The state starts EMPTY: a Stage is a VALUE, and
   * driving the same value twice must not share one array between
   * the runs */
  def chunked[T](size: Int): Stage[T, Chunk[T], Unit] = {
    val batched: Stage[T, Chunk[T], (ChunkBuf[T], Int)] =
      transduce[T, Chunk[T], (ChunkBuf[T], Int)]((null, 0))((acc, t) => {
        val (held, n) = acc
        val buf = if (held == null) ChunkBuf[T](size) else held
        buf(n) = t
        if (n + 1 < size) pure((buf, n + 1))
        else tell[T, Chunk[T]](buf.chunk).map(_ => (null, 0))
      }, acc => {
        val (held, n) = acc
        if (n == 0) pure(acc) else tell[T, Chunk[T]](held.take(n)).map(_ => acc)
      })
    batched.map(_ => ())
  }

  /** flatten chunks back into elements */
  def unchunk[T]: Stage[Chunk[T], T, Unit] =
    await[Chunk[T], T].flatMap {
      case Some(c) =>
        def emit(i: Int): Stage[Chunk[T], T, Unit] =
          if (i >= c.length) unchunk[T]
          else tell[Chunk[T], T](c(i)).flatMap(_ => emit(i + 1))
        emit(0)
      case None => pure(())
    }
}

/**
 * UTF-8 lines out of a byte stream — the framer every line protocol
 * shares. Framing happens on BYTES, before decoding: a chunk boundary
 * can fall inside a multi-byte UTF-8 sequence, and a newline byte
 * cannot, so splitting bytes first and decoding whole lines after is
 * both simpler and correct.
 */
object Lines {
  import java.nio.charset.StandardCharsets.UTF_8

  /** bytes in, lines out; CRLF stripped; a trailing line without a
   * newline is still a line */
  def stage: Stage[Chunk[Byte], String, Unit] = {
    def spill(buf: Array[Byte]): Stage[Chunk[Byte], String, Array[Byte]] = {
      var i = 0
      while (i < buf.length && buf(i) != '\n'.toByte) i += 1
      if (i >= buf.length) pure(buf)
      else {
        val line = new String(buf, 0, if (i > 0 && buf(i - 1) == '\r') i - 1 else i, UTF_8)
        Stage.tell[Chunk[Byte], String](line)
          .flatMap(_ => spill(java.util.Arrays.copyOfRange(buf, i + 1, buf.length)))
      }
    }

    val framed: Stage[Chunk[Byte], String, Array[Byte]] =
      Stage.transduce[Chunk[Byte], String, Array[Byte]](Array.empty[Byte])(
        (buf, c) => spill(buf ++ c.toArray),
        rest =>
          if (rest.isEmpty) pure(rest)
          else Stage.tell[Chunk[Byte], String](new String(rest, UTF_8)).map(_ => Array.empty[Byte]))

    framed.map(_ => ())
  }
}
