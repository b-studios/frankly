package examples

import frankly._

object drunkflip extends App {

  case object Flip extends Op[Boolean]
  case class Raise[A](msg: String) extends Op[A]



  def collect[R] : Eff[R] => Eff[List[R]] = handle {
    case Flip -> resume => for {
      ts <- collect { resume(true) }
      fs <- collect { resume(false) }
    } yield ts ++ fs
    case Return(x) => pure { List(x) }
  }

  def maybe[R] : Eff[R] => Eff[Option[R]] = handle {
    case Raise(msg) -> resume => pure(None)
    case Return(x) => pure { Some(x) }
  }

  def drunkflip = for {
    b <- !Flip
    r <- if (b) !Flip else !Raise("dropped coin")
  } yield if (r) "Heads" else "Tails"

  println {
    run {
      collect {
        maybe {
          drunkflip
        }
      }
    }
  }

  println {
    run {
      maybe {
        collect {
          drunkflip
        }
      }
    }
  }
}