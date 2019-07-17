import scala.annotation.tailrec

// This is just a draft of an _approximate_ effect system, inspired by
//    http://ps.informatik.uni-tuebingen.de/publications/brachthaeuser19effekt/
//
// -----------
//
// Disclaimer:
//
//   By no means is the effect system here safe or sound.
//
// Examples of unsoundness:
// 1) handlers might _claim_ they handle effects but actually don't pattern
//    match on clauses (this is not checked)
// 2) handlers might forget return clauses (this is not checked)
// 3) Effect encapsulation is not guaranteed. Effects might be handled accidentally in
//    the presence of effect polymorphism.
//
// That said, it still might be useful to have an approximate effect system.
package object typed {

  type Pure = Any

  trait Eff[+A, -E] {
    def map[B](f: A => B): Eff[B, E]
    def flatMap[B, E2](f: A => Eff[B, E2]): Eff[B, E & E2]
    def andThen[B, E2](f: Eff[B, E2]): Eff[B, E & E2]
  }

  def pure[A](a: A): Eff[A, Pure] = ???

  trait Op[A, E] {
    def send(): Eff[A, E] = ???
  }

  trait Clause[R, Fx]

  object Return {
    def unapply[A, Fx](c: Clause[A, Fx]): Option[A] = ???
  }

  type ->[-A, +B] = PartialFunction[A, B]
  object -> {
    def unapply[A, X, Fx](c: Clause[A, Fx]): Option[(Op[X, _], X => Eff[A, Fx])] = ???
  }

  def handle[R, E, Res, Fx](h: Clause[R, E & Fx] -> Eff[Res, Fx]): Eff[R, E & Fx] => Eff[Res, Fx] = ???

  case class Flip() extends Op[Boolean, Flip.type]

  def collect[R, FX]: Eff[R, Flip.type & FX] => Eff[List[R], FX] = handle {
    case Flip() -> resume => for {
      ts <- collect { resume(true) }
      fs <- collect { resume(false) }
    } yield ts ++ fs
    case Return(v) => pure { List(v) }
  }

  trait State[S] extends Op[S, State.type]
  object State {
    case class Get() extends State[Int]
    case class Put(n: Int) extends State[Unit]
  }
  import State._

  // it's not really checked, that the state effect is handled in the clauses
  def state[A, E](s: Int): Eff[A, State.type & E] => Eff[A, E] = handle {
    case Get() -> k   => state(s) { k(s) }
    case Put(s2) -> k => state(s2) { k(()) }
  }

  def ex: Eff[Int, State.type & Flip.type] = for {
    x <- Get().send()
    y <- Get().send()
    _ <- Flip().send()
    _ <- Put(x + y).send()
  } yield x + y

  val nested: Eff[List[Int], Pure] =
    state(0) {
      collect[Int, State.type] { ex } // the effect annotation is necessary
    }

}
