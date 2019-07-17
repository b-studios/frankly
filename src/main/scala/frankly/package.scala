import scala.annotation.tailrec

package object frankly {

  // USER API
  // ========

  sealed trait Eff[+A] { outer =>
    def map[B](f: A => B): Eff[B] = flatMap(a => pure(f(a)))
    def flatMap[B](f: A => Eff[B]): Eff[B]
    def andThen[B](f: Eff[B]): Eff[B] = flatMap { _ => f }
    def foreach(f: A => Unit): Eff[Unit] = map(f)
    def >>[B](f: Eff[B]): Eff[B] = andThen(f)

    def withFilter(p: A => Boolean): Eff[A] = flatMap {
      case a if p(a) => pure(a)
      case a => Error(new Throwable("Could not match " + a))
    }
  }

  trait Op[R] {
    def unary_! : Eff[R] = Effect(this, Continuation.empty)
  }

  opaque type Clause[R] = Eff[R]
  type ->[-A, +B] = PartialFunction[A, B]
  object -> {
    def unapply[A, X](c: Clause[A]): Option[(Op[X], X => Eff[A])] = c match {
      case Effect(Trampoline(t), k) => None // don't handle trampoline
      case Effect(op: Op[X], k) => Some(op, k)
      case _ => None
    }
  }

  // Return could be an effect, but then pattern matching with `->` would also bind
  // the continuation.
  object Return {
    def unapply[A](c: Clause[A]): Option[A] = c match {
      case p: Pure[A] => Some(p.value)
      case _ => None
    }
  }

  def pure[A](a: => A): Eff[A] = Pure(a)

  @tailrec
  def run[A](c: Eff[A]): A = c match {
    case v: Pure[A] => v.value
    case Effect(Trampoline(c), k) => run[A](k(c()))
    case Error(e) => throw e
    case c => sys error s"unhandled operation $c"
  }

  // shallow handle
  def handle[R, Res](h: Clause[R] -> Eff[Res]): Eff[R] => Eff[Res] = {
    case clause if h.isDefinedAt(clause) => trampoline(clause, h)
    case Effect(op, k) => Effect(op, Continuation { r => handle(h) { k(r) } })

    // handlers might forget to implement return clauses, which will fail at runtime here
    // if R = Res that's ok. Otherwise its unsound
    case v: Pure[Res] => v
    case e: Error => e
  }

  // first draft of multi handlers!
  def handle[R, S, Res](h: (Clause[R], Clause[S]) -> Eff[Res]): (Eff[R], Eff[S]) => Eff[Res] = {
    case clause @ (p1, p2) if h.isDefinedAt(clause) => trampoline(clause, h)
    case (Effect(op, k), p2) =>
      Effect(op, Continuation { r => handle(h)(k(r), p2) })
    case (p1, Effect(op, k)) =>
      Effect(op, Continuation { r => handle(h)(p1, k(r)) })
    case (_: Pure[_], _) | (_, _: Pure[_]) =>
      sys error "multi handlers need to define return clauses"
    case (Error(e), _) => Error(e)
    case (_, Error(e)) => Error(e)
  }

  def handle[R1, R2, R3, Res](h: (Clause[R1], Clause[R2], Clause[R3]) -> Eff[Res]): (Eff[R1], Eff[R2], Eff[R3]) => Eff[Res] = {
    case clause @ (p1, p2, p3) if h.isDefinedAt(clause) => trampoline(clause, h)
    case (Effect(op, k), p2, p3) =>
      Effect(op, Continuation { r => handle(h)(k(r), p2, p3) })
    case (p1, Effect(op, k), p3) =>
      Effect(op, Continuation { r => handle(h)(p1, k(r), p3) })
    case (p1, p2, Effect(op, k)) =>
      Effect(op, Continuation { r => handle(h)(p1, p2, k(r)) })
    case (_: Pure[_], _, _) | (_, _: Pure[_], _) | (_, _, _: Pure[_]) =>
      sys error "multi handlers need to define return clauses"
    case (Error(e), _, _) => Error(e)
    case (_, Error(e), _) => Error(e)
    case (_, _, Error(e)) => Error(e)
  }



  // IMPLEMENTATION DETAILS
  // ======================

  private class Pure[A](v: => A) extends Eff[A] {
    lazy val value = v
    def flatMap[B](f: A => Eff[B]): Eff[B] = trampoline(value, f)
    override def toString: String = s"Pure(${value})"
  }

  private case class Error(err: Throwable) extends Eff[Nothing] {
    def flatMap[B](f: Nothing => Eff[B]) = this
  }
  private case class Effect[X, A](op: Op[X], k: Continuation[X, A]) extends Eff[A] {
    def flatMap[B](f: A => Eff[B]): Eff[B] = Effect(op, k append f)
  }

  private case class Trampoline[A](value: () => A) extends Op[A]

  @inline
  private def trampoline[R, Res](r: => R, k: R => Eff[Res]): Eff[Res] =
    Effect(Trampoline(() => r), Continuation { k })
}