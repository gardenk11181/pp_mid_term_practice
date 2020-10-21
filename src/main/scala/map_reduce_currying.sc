def mapReduce(reduce: (Int,Int)=>Int, iniVal: Int)(f: Int=>Int)(a:Int, b:Int): Int = {
  if(a<=b) reduce(f(a),mapReduce(reduce,iniVal)(f)(a+1,b)) else iniVal
}

def sum = mapReduce((x,y) => x+y, 0) _
sum(x=>x*x)(1,3)

// why val is better?
val product = mapReduce((x,y) => x*y, 1) _
product(x => x*x)(1,3)
// maybe make new instance once time