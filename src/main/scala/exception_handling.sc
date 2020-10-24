class factRangeException(val arg: Int) extends Exception

def fact(n: Int): Int = {
  if(n <0) throw new factRangeException(n)
  else if(n==0) 1
  else n*fact(n-1)
}

try {
  println(fact(3))
  println(fact(-100))
} catch {
  case e: factRangeException => {
    println("fact range error: "+ e.arg)
  }
}