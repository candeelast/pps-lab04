package u04lab
import u03.Sequences.*
import Sequence.*
import u03.Optionals.*
import u03.Optionals.Optional.Just
import u04lab.Ex5Traversable.Traversable

/*  Exercise 5: 
 *  - Generalise by ad-hoc polymorphism logAll, such that:
 *  -- it can be called on Sequences but also on Optional, or others... 
 *  -- it does not necessarily call log, but any function with analogous type
 *  - Hint: introduce a type class Traversable[T[_]]], capturing the ability of calling a
 *    "consumer function" on all elements (with type A) of a datastructure T[A] 
 *    Note Traversable is a 2-kinded trait (similar to Filterable, or Monad)
 *  - Write givens for Traversable[Optional] and Traversable[Sequence]
 *  - Show you can use the generalisation of logAll to:
 *  -- log all elements of an Optional, or of a Traversable
 *  -- println(_) all elements of an Optional, or of a Traversable
 */

object Ex5Traversable:

  trait Traversable[T[_]]:
    def logAll[A](t: T[A]): Unit

  def log[A](a: A): Unit = println("The next element is: "+a)



  given Traversable[Sequence] with
    def logAll[A](t: Sequence[A]): Unit = t match
      case Cons(h, t) => log(h); logAll(t)
      case _ => ()


  given Traversable[Optional] with
    def logAll[A](t: Optional[A]): Unit = t match
      case Optional.Just(a) => println(a)
      case _ => ()

  def generalLog[A, T[_] : Traversable](cont: T[A]): Unit =
    val traversableInstance = summon[Traversable[T]]
    traversableInstance.logAll(cont)

@main def tryTraversable =
  val seq = Cons(10, Cons(20, Cons(30, Nil())))
  val traversableSeq = summon[Traversable[Sequence]]
  traversableSeq.logAll(seq) // Logs: "The next element is: 10", etc.

  val opt = Just(42)
  val traversableOpt = summon[Traversable[Optional]]
  traversableOpt.logAll(opt) // Logs: "42"

  val emptyOpt = Optional.Empty()
  traversableOpt.logAll(emptyOpt) // Logs nothing