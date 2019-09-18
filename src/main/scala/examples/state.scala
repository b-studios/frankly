package examples

import frankly._

object state extends App {

  case object Get extends Op[Int]
  case class Put(n: Int) extends Op[Unit]


  // Some example handlers
  // ---------------------

  def always42[R]: Eff[R] => Eff[R] = handle {
    case Get -> k => always42 { k(42) }
  }

  def collect[R]: Eff[R] => Eff[List[Int]] = handle {
    case Return(v) => pure(Nil)
    case Put(n) -> resume => for {
      xs <- collect { resume(()) }
    } yield n :: xs
  }

  def state[R](n: Int): Eff[R] => Eff[R] = handle {
    case Get -> resume => state(n) { resume(n) }
    case Put(n) -> resume => state(n) { resume(()) }
  }

  // piping puts to gets
  def pipe: (Eff[Unit], Eff[Unit]) => Eff[Unit] = handle {
    case (Get -> kGet, Put(n) -> kPut) => pipe(kGet(n), kPut(()))
    case (Put(n) -> kPut, Get -> kGet) => pipe(kPut(()), kGet(n))
    case _ => pure(())
  }


  // Some programs
  // -------------

  def statetest: Eff[Unit] = for {
    x <- !Get
    _ = println(x)
    y <- !Put(x + 1)
    x <- !Get
    _ = println(x)
    y <- !Put(x + 1)
        x <- !Get
    _ = println(x)
    y <- !Put(x + 1)
    x <- !Get
    _ = println(x)
    y <- !Put(x + 1)
    x <- !Get
    _ = println(x)
    y <- !Put(x + 1)
    x <- !Get
    _ = println(x)
    y <- !Put(x + 1)
    x <- !Get
    _ = println(x)
    y <- !Put(x + 1)
  } yield ()

  run { state(0) { statetest } }

  val ex = for {
    x <- !Get
    _ <- !Put(x + 1)
    y <- !Get
    _ <- !Put(y + 1)
  } yield x + y

  def getN(n: Int): Eff[Int] =
    if (n < 1) pure(0)
    else for {
      x <- !Get
      r <- getN(n - 1)
    } yield x + r

  assert { run { collect { for {
    r <- always42 { ex }
    _ <- !Put(r)
  } yield () } } == List(43, 43, 84) }

  assert { run { state(1) { ex } } == 3 }

  println { run { always42 { getN(10000) }}}

  println { run {
    pipe(for {
      x <- !Get
      _ = println("Got " + x)
      y <- !Get
      _ = println("Got " + y)
      _ <- !Put(x + y)
    } yield (), for {
      _ <- !Put(1)
      _ <- !Put(2)
      r <- !Get
      _ = println(r)
    } yield ())
  }}

}