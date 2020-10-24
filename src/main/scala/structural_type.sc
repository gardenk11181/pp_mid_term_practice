val foo = new {
  val a = 1+2
  def b = a+1
  def f(x : Int) = b+x+1
}

type Foo = {val a: Int; def b: Int; def f(x:Int):Int}

def g(x: Foo) = {
  val gn = 100
  x.f(gn)
}

g(foo)