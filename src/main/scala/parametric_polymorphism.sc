import scala.annotation.tailrec

def id[A](x:A) : A =
  x

id(3)
id("A")

@tailrec
def applyN[A] (f: A=>A, n: Int, x: A) :A =
  n  match {
    case 0 => x
    case _ => applyN(f,n-1,f(x))
  }

applyN((x:Int)=>x+1,1000000,1)
applyN((x:String)=>x+"!",100,"!")

// scala encode function
// as object including function named 'apply'

object applyN2 {
  @tailrec
  def apply[A](f:A=>A,n:Int,x:A): A =
    n match {
      case 0 => x
      case _ => applyN2(f,n-1,f(x))
    }
}

applyN2((x:Int)=>x+1,1000000,1)
// applyN2.apply[Int]((x:Int)=>x+1,1000000,1)