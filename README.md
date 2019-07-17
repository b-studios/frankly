# Frankly the Frank-en-Scala
A quick draft port of the Frank language to Scala -- or like the french would say: "Frank-en-Scala"

Disclaimer: Don't expect to much. It's just an experimental draft. Frankly is an effect library inspired 
by [Frank](https://github.com/frank-lang) with support for _shallow handlers_ and _multi handlers_.

Things we frankly don't support:

- an effect system. Use effects on your own risk
- multi handlers with more than 3 effectful programs (of course we could support that).
- [adaptors / masks / injects](http://homepages.inf.ed.ac.uk/slindley/papers/frankly-draft-february2019.pdf) -- all kinds of forwarding and effect adaption trickery

## Example
Here is a quick example of shallow multi-handlers in frankly. The effect operations:
```scala
case object Get extends Op[Int]
case class Put(n: Int) extends Op[Unit]
```
The effect handler
```scala
def pipe: (Eff[Unit], Eff[Unit]) => Eff[Unit] = handle {
  case (Get -> kGet, Put(n) -> kPut) => pipe(kGet(n), kPut(()))
  case (Put(n) -> kPut, Get -> kGet) => pipe(kPut(()), kGet(n))
  case _ => pure(())
}
```
A program using both, the handler, and the effect operations:
```scala
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
```

It yields:
```
Got 1
Got 2
3
()
```

## Implementation 

It is implemented based on freer monads, just like [Atnos-Eff](https://github.com/atnos-org/eff). Actually,
we are using their implementation of type-aligned-sequences.

