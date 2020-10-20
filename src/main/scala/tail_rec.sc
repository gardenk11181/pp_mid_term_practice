import scala.annotation.tailrec
import scala.util.control.TailCalls.{TailRec, done, tailcall}

def sumBasic(n: Int): Int = {
  if(n==0) 0
  else n+sumBasic(n-1)
}
//sumBasic(100000)

def sum(n: Int): Int = {
  @annotation.tailrec
  def sumIter(n: Int, sum: Int) : Int = {
    if(n==0) sum
    else sumIter(n-1,n+sum)
  }
  sumIter(n,0)
}

sum(1000000)


def mutSum(n: Int): Int = {
  def sumIter1(n: Int, sum: Int) : TailRec[Int] = {
    if(n==0) done(sum)
    else tailcall(sumIter2(n-1,sum + n))
  }

  def sumIter2(n: Int, sum: Int) : TailRec[Int] = {
    if(n==0) done(sum)
    else tailcall(sumIter1(n-1, sum + 2*n))
  }

  sumIter1(n,0).result
}

mutSum(100000)

