import scala.annotation.tailrec
// calculate square-root by newton method

def sqrt(x: Double): Double = {

  def isGoodEnough(guess: Double, x: Double): Boolean = {
    val improved = (guess*guess-x)/x
    improved >= -0.1 && improved <= 0.1
  }

  def improve(guess: Double, x: Double): Double = {
    (guess + x/guess) / 2
  }

  @tailrec
  def sqrtIter(guess: Double, x: Double) : Double = {
    if(isGoodEnough(guess,x)) guess
    else sqrtIter(improve(guess,x),x)
  }

  sqrtIter(1,x)
}

sqrt(2)