sealed trait List1[+A]
case object Nil extends List1[Nothing]
case class Cons[+A](head : A, tail : List1[A]) extends List1[A]

object List1 {
  def sum(ints : List1[Int]) : Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }
  def apply[A](as: A*): List1[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def main(args : Array[String]) : Unit =
  {
    List1(10, 20)
  } 

}
