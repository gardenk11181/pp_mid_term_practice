import scala.annotation.tailrec
import scala.util.control.TailCalls.{TailRec, done, tailcall}

def fact(n: Int): Int = {
  @tailrec
  def factTail(cont: Int => TailRec[Int], n: Int) : TailRec[Int] = {
    if(n==1) tailcall(cont(1))
    else factTail((x) => tailcall(cont(n*x)),n-1)
  }
  val tail: Int => TailRec[Int] = x => done(x)
  factTail(tail,n).result
}

fact(10)

def sum(n: Int): Int = {
  @tailrec
  def sumTail(cont: Int => TailRec[Int], n: Int): TailRec[Int] =
    if(n==0) tailcall(cont(0))
    else sumTail(x => tailcall(cont(n+x)),n-1)

  val tail: Int => TailRec[Int] = x => done(x)
  sumTail(tail,n).result
}

sum(10000)