package okay

type Pure[A] = A
type Produce[A] = A ! Pure
def produce[A](a: A): Produce[A] = raise(a)

given Put[Produce] with {
  override inline def put[A](a: A): A \ Produce[A] =
    shift(produce(a).flatMap(_))
}

given Handler[Pure] with
  override def apply[A, B](a: A): A \ B = _(a)
