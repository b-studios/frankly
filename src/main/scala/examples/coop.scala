package examples

import frankly._

// Coop Multitasking example adapted from
//   https://github.com/frank-lang/frank/blob/master/tests/should-pass/coop/coop-factored.fk
object coop extends App {

  trait Co[A] extends Op[A]
  case object Yield extends Co[Unit]
  case class Fork(prog: Eff[Unit]) extends Co[Unit]

  case class Print(msg: String) extends Op[Unit]

  // TODO fix evaluation order when using real side effects within pure { ... }
  def log(msg: String): Eff[Unit] = !Print(msg)

  def console[R]: Eff[R] => Eff[R] = handle {
    case Print(msg) -> k => println(msg); console { k(()) }
  }

  def test: Eff[Unit] = for {
    _ <- log("M1 ")
    _ <- !Fork { for { _ <- log("A1 "); _ <- !Yield; _ <- log("A2 ") } yield () }
    _ <- log("M2 ")
    _ <- !Yield
    _ <- !Fork { for { _ <- log("B1 "); _ <- !Yield; _ <- log("B2 ") } yield () }
    _ <- !Yield
    _ <- log("M3 ")
    _ <- !Yield
    _ <- log(".")
  } yield ()

  type Proc = Eff[Unit] // using Queue

  // we specialize Queue to Proc since typing gets ugly otherwise. In particular,
  // Dequeue[S]() would be like `Raise[A]` - how can we come up with an inhabitant of
  // S at the handling site?
  trait Queue[S] extends Op[S]
  case class Enqueue(p: Proc) extends Queue[Unit]
  case class Dequeue() extends Queue[Option[Proc]]



  def pushProc: Proc => Proc = p => !Enqueue(p)
  def popProc: Eff[Option[Unit]] =
    !Dequeue() flatMap {
      case (Some(p)) => p map { Some(_) }
      case None => pure(None)
    }

  def popProcs: Eff[Unit] = popProc flatMap {
    case Some(unit) => popProcs
    case None => pure(())
  }

  def roundRobin: Eff[Unit] => Eff[Unit] = handle {
    case Yield -> k   => pushProc { roundRobin(k(())) } >> popProcs
    case Fork(p) -> k => pushProc { roundRobin(p) } >> roundRobin(k(()))
    case Return(())   => popProcs
  }

  def zipQueue: List[Proc] => List[Proc] => Eff[Unit] => Eff[Unit] = ps => qs => handle {
    case Enqueue(q) -> k => zipQueue(ps)(q :: qs) { k(()) }
    case Dequeue() -> k => (ps, qs) match {
      case (Nil, Nil)     => zipQueue(Nil)(Nil) { k(None) }
      case (Nil, qs)      => zipQueue(qs.reverse)(Nil) { !Dequeue() flatMap k }
      case (p :: ps, qs)  => zipQueue(ps)(qs) { k(Some(p)) }
    }
    case Return(()) => pure(())
  }

  run { console {
    zipQueue(Nil)(Nil) {
      roundRobin {
        test
      }
    }
  }}

  //-- Expected results:
  //-- #desc   examples/coop-factored.fk
  //-- #return M1 M2 A1 A2 B1 M3 B2 .unit

  // some strictness tests:
  pure { println("This should not be printed") }
  pure { println("Neither should this") } flatMap { x => pure { println("Nor this") } }
}