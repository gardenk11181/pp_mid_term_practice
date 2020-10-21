def mapReduce(f: Int => Int, op: (Int,Int)=>Int, a:Int, b:Int, iniVal:Int) : Int = {
  if(a <= b) op(f(a),mapReduce(f,op,a+1,b,iniVal))
  else iniVal
}

mapReduce(x=>x, (x,y)=>x+y, 1,3,0)

def sum(f: Int => Int, a: Int, b:Int): Int = {
  mapReduce(f,(x: Int,y: Int)=>x+y, a,b,0)
}

def product(f: Int => Int, a: Int, b: Int): Int = {
  mapReduce(f, (x:Int, y:Int)=>x*y, a,b,1)
}

