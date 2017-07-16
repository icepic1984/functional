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
    case Nil => sys.error("tail of empty list")
  }

  def setHead[A]( a : List1[A], x : A) : List1[A] = a match {
    case Cons(_, xs) => Cons(x,xs)
    case Nil => sys.error("setHead of empty list")
  }

  def drop[A](list : List1[A], n : Int) : List1[A] = {
    if( n <= 0)
      list
    else list match {
      case Cons(_, xs) => drop(xs, n-1)
      case Nil => Nil
    }
  }

  def dropWhile[A](list : List1[A], f: A => Boolean) : List1[A] = list match{
      case Cons(x, xs) if f(x) => dropWhile(xs,f) 
      case _ => list
  }

  def append[A](list1 : List1[A], list2 : List1[A]) : List1[A] = 
    list1 match {
      case Nil => list2
      case Cons(h,t) => Cons(h, append(t, list2))
  }

  def init[A](list : List1[A]) : List1[A] = list match{
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
    case Nil => Nil

  }

  def length[A](list : List1[A]) : Int = {
    foldRight(list,0)((_,y) => 1 + y)
  }

  def foldRight[A,B](list : List1[A], z : B)(f : (A,B) => B) : B = list match{
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs,z)(f))
  }

  @annotation.tailrec
  def foldLeft[A,B](list : List1[A], z : B)(f: (B,A) => B) : B = list match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs,f(z,x))(f)
  }

  def append2[A](list1 : List1[A], list2 : List1[A]) : List1[A] = {
    List1.foldRight(list1, list2)(Cons(_,_))
  }

  def sum2(list : List1[Int]) : Int = {
    List1.foldLeft(list,0)( _ + _)
  }

  def product2(list : List1[Double]) : Double = {
    List1.foldLeft(list,1.0)(_ * _)
  }

  def reverse[A](list : List1[A]) : List1[A] = {
    List1.foldLeft(list, Nil:List1[A])((h,t) => Cons(t,h))
  }

  def concat[A](list : List1[List1[A]] ) = {
    List1.foldRight(list, Nil:List1[A])(append)
  }

  def transform(list : List1[Int]) : List1[Int] = {
    List1.foldRight(list,Nil:List1[Int])((h,t) => Cons(1+h,t))
  }

  def map[A,B](list : List1[A])(f : A => B) : List1[B] = {
    List1.foldRight(list, Nil:List1[B])((h,t) => Cons(f(h),t))
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
    List1.dropWhile(List1(1,2,3,4,5), (x : Int) => x < 3)  
    List1.foldRight(Cons(1,Cons(2,Cons(3,Nil))), Nil:List1[Int])(Cons(_,_))

    List1.concat(List1(List1(10,20),List1(30,40),List1(50,60)))


  } 

}
