def sum(f: Int => Int): (Int,Int)=>Int = {
  def sumF(a: Int, b: Int): Int = {
    if (a<=b) f(a) + sumF(a+1,b) else 0
  }
  sumF _
}

def sumLinear: (Int, Int)=> Int = sum((n: Int) => n)
def sumSquare = sum((n)=>n*n)
def sumCube = sum(n => n*n*n)

sumLinear(1,2)

def sumImproved(f: Int => Int)(a: Int, b: Int) : Int = {
  if(a<=b) f(a) + sumImproved(f)(a+1,b) else 0
}

def sumLinearImproved = sumImproved(n=>n) _

// this is non-efficient because
// function type has to be made for each iteration
def sumNoEfficient(f: Int => Int) : (Int, Int)=>Int = {
  (a,b) => if(a<=b) f(a)+sum(f)(a+1,b) else 0
}

// currying using anonymous functions

def foo(x: Int, y: Int, z: Int)(a:Int, b:Int): Int = {
  x+y+z+a+b
}

val f1 = (x: Int, z: Int, b: Int) => foo(x,1,z)(2,b)
val f2 = foo(_:Int,1,_:Int)(2,_:Int)
val f3 = (x: Int, z: Int) => ((b: Int) => foo(x,1,z)(2,b))

f1(1,2,3)
f2(1,2,3)
f3(1,3)(2)

