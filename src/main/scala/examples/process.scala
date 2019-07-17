package examples

import frankly._

object process extends App {

  sealed trait Process[A] extends Op[A]
  case class Fork[R](p1: Eff[R], p2: Eff[R]) extends Process[R]
  case class Atomic[R](region: Eff[R]) extends Process[R]
  case class Act[R](action: Eff[R]) extends Process[R]

  case class Resume[R](action: Eff[R]) extends Op[R]

  sealed trait Console[A] extends Op[A]
  case class Print(msg: String) extends Console[Unit]

  // TODO implement
  def scheduler[R]: Eff[R] => Eff[R] = handle {
    case Act(action) -> resume => ???
    case Fork(p1, p2) -> resume => ???
    case Atomic(action) -> resume => ???
  }

  def interleave[R]: Boolean => (Eff[R], Eff[R]) => Eff[R] = flag => handle {
    case (Act(p) -> k, p2) if flag => ???
    case (p1, p2) if !flag => ???
  }


  def example = !Fork(for {
    _ <- !Act { !Print("Hello") }
    _ <- !Act { !Print("World") }
  } yield (), for {
    _ <- !Atomic { for {
        _ <- !Act { !Print("Check") }
      _ <- !Act { !Print("Check!") }
    } yield () }
    _ <- !Act { !Print("One") }
    _ <- !Act { !Print("Two") }
  } yield ())
}