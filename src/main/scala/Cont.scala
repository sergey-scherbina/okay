package okay

infix type /[A, B] = A => B
infix type \[A, R] = A / R / R
infix type ^[A, R] = A / A / R

// (A => B) => C
extension [A, B, C](c: A / B / C)
  inline infix def /(f: A / B): C = c(f)
  
inline def shift[A, B, C](c: (A => B) => C): A / B / C = c
inline def reset[A, R](c: A ^ R): R = c / identity

type Cont[A, B, C] = A / B / C

given ParaMonad[Cont] {
  inline override def pure[A, R](a: A): A \ R = _(a)
  extension [A, B, C](m: A / B / C)
    inline override def flatMap[A1, B1]
    (f: A => A1 / B1 / B): A1 / B1 / C =
      k => m(a => f(a)(k))
}

infix type <<[A, R] = A / R / (A / R)
inline def <<[A, R]: A << R = shift(identity)
def loop[A, R](f: A << R): A / R = f / (loop(f)(_))
extension [A](a: A) inline def apply[R](f: A << R): R = loop(f)(a)
