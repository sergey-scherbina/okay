package okay

import scala.annotation.tailrec
import okay.!.*

/**
 * The consumer side of a pipeline: await the next element of type V.
 * A consumer is a program with the Take effect — the exact dual of a
 * writer (tell pushes an element out, await pulls one in), and pipe
 * below is the classic coroutine pairing of the two by delimited
 * continuations: no channel, no buffer, no materialized list — each
 * await transfers control to the producer for exactly one element.
 */
enum Take[V, +A]:
  /** the next element, or None — the producer has ended */
  case Await[V]() extends Take[V, Option[V]]

object Take:
  /** the next element, or None at the end of the input */
  inline def await[V]: Option[V] ! Take % V = effect(Await())

/**
 * Connect a producer to a consumer: each await meets the next told
 * value, control bouncing between the two programs one element at a
 * time. The consumer drives — a finite consumer ends an INFINITE
 * producer (only the asked elements are ever computed); when the
 * producer ends first, every further await answers None. The
 * consumer's answer is the result; the producer's surplus is dropped,
 * its answer ignored. Fully typed by the Take/Writer GADTs — no casts.
 */
def pipe[W, A, B](p: A ! Writer % W)(c: B ! Take % W): B = {
  @tailrec def loop(p: A ! Writer % W, c: B ! Take % W): B = c.resume match
    case Pure(b) => b
    case Effect(Take.Await()) => Writer.uncons(p).toOption.map(_._1)
    case Bind(Effect(Take.Await()), k) => Writer.uncons(p) match
      case Right((w, rest)) => loop(rest, k(Some(w)))
      case Left(_) => loop(p, k(None))

  loop(p, c)
}

/**
 * The same pipe for a producer performing arbitrary effects G: the
 * consumer still drives, and the G-operations met between elements
 * are carried into the answer — the result is a program in G.
 * (Structured effects are handled over the producer first — handlers
 * are stream transformers; the Handler-able residue is what remains.)
 */
def pipe[W, A, B, G[+_] : TypeableK](p: A ! Writer % W + G)(c: B ! Take % W): B ! G = {
  def loop(p: A ! Writer % W + G, c: B ! Take % W): B ! G = c.resume match
    case Pure(b) => pure(b)
    case Effect(Take.Await()) => Writer.uncons(p).map(_.toOption.map(_._1))
    case Bind(Effect(Take.Await()), k) => Writer.uncons(p).flatMap:
      case Right((w, rest)) => loop(rest, k(Some(w)))
      case Left(_) => loop(p, k(None))

  loop(p, c)
}
