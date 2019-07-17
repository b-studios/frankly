package frankly

import scala.annotation.tailrec

/**
 * The following is adapted from Eric Torreborre's Continuation type in Atnos-Eff
 * type aligned sequence as a vector of frames
 *
 *     https://github.com/atnos-org/eff/blob/master/shared/src/main/scala/org/atnos/eff/Continuation.scala
 */
private case class Continuation[A, B](functions: Vector[Any => Eff[Any]]) extends (A => Eff[B]) {
  def append[C](f: B => Eff[C]): Continuation[A, C] =
    Continuation(functions :+ f.asInstanceOf[Any => Eff[Any]])

  def apply(a: A): Eff[B] = {
    @tailrec
    def go(fs: Vector[Any => Eff[Any]], v: Any): Eff[B] = fs match {
      case vec if vec.isEmpty => Pure(v.asInstanceOf[B])

      case f +: rest =>
        val fv = f(v)
        if (rest.isEmpty) {
          fv.asInstanceOf[Eff[B]]
        } else {
          fv match {
            case p: Pure[A] =>
              go(rest, p.value)

            case Effect(op, k) =>
              Effect(op, Continuation(k.functions ++ rest))
          }
        }
    }
    go(functions, a)
  }
}
private object Continuation {
  def empty[A]: Continuation[A, A] = Continuation(Vector.empty)

  def apply[A, B](f: A => Eff[B]): Continuation[A, B] =
    Continuation(Vector.apply(f.asInstanceOf[Any => Eff[Any]]))
}