object Functional {

  def fac(n : Int) : Int = {
    @annotation.tailrec
    def go(n : Int, acc : Int) : Int = {
      if(n <= 0) acc
      else go(n-1,n*acc)
    }
    go(n,1)
  }

  def isSorted[A](as : Array[A], f : (A,A) => Boolean) : Boolean = {
    def loop(n : Int) : Boolean = {
      if(as.length < 2) false
      else if(n >= as.length - 2 ) f(as(n),as(n+1))
      else if (!f(as(n),as(n+1))) false
      else loop(n+1)
    }
    loop(0)
  }

  def findFirst[A](ss : Array[A], p: A => Boolean) : Int = {
    @annotation.tailrec
    def loop(n : Int) : Int = {
      if(n >= ss.length) -1
      else if(p(ss(n))) n
      else loop(n+1)
    }
    loop(0)
  }

  def partial[A,B,C](a : A, f:(A,B) => C): B => C =
    (b : B) => f(a,b)

  def curry[A,B,C](f: (A,B) => C): A => (B => C) = {
    (a : A) => partial1(a,f)
  }
  
  def uncurry[A,B,C](f: A => B => C) : (A, B) => C = {
    (a : A, b : B) => f(a)(b)
  }

  def compose[A,B,C](f : B => C, g : A => B ) : A => C = {
    (a : A) => f(g(a))
  }

}

