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

  def tail[A]( a : List1[A]) :  List1[A] = a match {
    case Cons(_, xs) => xs
  }


  def main(args : Array[String]) : Unit =
  {
    List1(10, 20)
    val x = List1(1,2,3,4,5) match {
      case Cons(x, Cons(2,Cons(4,_))) => x
      case Nil => 42
      case Cons( x, Cons(y,Cons(3, Cons(4,_)))) => x + y
      case Cons(h, t) =>  h + sum(t)
      case _ => 101
    }
    println(x)
  } 

}
