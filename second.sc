

object Prog extends App{
  val a = 0
  def parial[A,B,C](a:A,f:(A,B)=>C): B=>C = f(a,_)
  def cur[A,B,C](f:(A,B)=>C): A =>(B=>C) = (a: A) => f(a,_)
  def ucurry[A,B,C](f:A=>B=>C):(A,B)=>C = (a:A,b:B) => f(a)(b)
  def compose[A,B,C](f:B=>C,g:A=>B):A=>C = composea(f,g,_)
  def composea[A,B,C](f:B=>C,g:A=>B, a:A) = f(g(a))
  trait Monoid[A] {
    def op(a1: A, a2: A): A
    def zero: A
  }
  val stringMonoid = new Monoid[String] {
    def op(a1:String, a2:String) = a1 + a2
    def zero = ""
  }
  val intAdd = new Monoid[Int] {
    def op(a1:Int,a2:Int) = a1+a2
    def zero = 0
  }
  val intMul = new Monoid[Int] {
    def op(a1:Int,a2:Int) = a1+a2
    def zero = 1
  }
  val bOr = new Monoid[Boolean] {
    def op(a1:Boolean,a2:Boolean) = a1||a2
    def zero = true
  }

  val bAnd = new Monoid[Boolean] {
    def op(a1:Boolean,a2:Boolean) = a1&&a2
    def zero = false
  }
  def optionMon[A] = new Monoid[Option[A]]{
    def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2
    def zero = None
  }
  def EndoMonoid[A]: Monoid[A => A] ={
    def opu(a: A=>A, b: A=>A, c:A): A = a(b(c))
    new Monoid[A=>A] {
      def op(a1: A=>A, a2: A=>A) = opu(a1,a2,_)
      def zero = x=>x
    }
  }

  def last(L:List[Any]):Any = {
    L match{
      case h1::Nil => h1
      case h2::tail => last(tail)
      case Nil => Nil
    }
  }
  def last_but_the_one(L:List[Any]):Any = L match{
    case h1::_::Nil => h1
    case _::tail => last_but_the_one(tail)
    case Nil => Nil
  }
  def nth(l: List[Any], n: Int):Any ={
    def loop(list:List[Any], k:Int): Any = {
      k==n match{case true => list.head; case _ => loop(list.tail,k+1)}
    }
    l match{
      case Nil=> Nil
      case h::Nil => h
      case _ => loop(l,0)}
  }
}

val a = Prog.last_but_the_one(List(0.2,15,1,3,8,-7))
val b = Prog.nth(List(1, 1, 2, 3, 5, 8),10)
