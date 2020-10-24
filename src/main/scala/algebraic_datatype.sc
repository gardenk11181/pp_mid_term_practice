import scala.annotation.tailrec

sealed abstract class IOption
case class INone() extends IOption
case class ISome(x: Int) extends IOption

def x: IOption = ISome(1)

sealed abstract class BTree
case class Leaf() extends BTree
case class Node(key: Int, left: BTree, right: BTree) extends BTree

sealed abstract class IList
case class INil() extends IList
case class ICons(x: Int, next:IList) extends IList

def length(x: IList): Int = {
  x match {
    case INil() => 0
    case ICons(x,next) => length(next)+1
  }
}

length(ICons(1,INil()))

// advanced pattern matching
def secondElement(x: IList): IOption = {
  x match {
//    case INil() | ICons(_,INil()) => INone()
//    case ICons(_,ICons(x,_)) => ISome(x)
//    case _ => INone()
    case ICons(_, ICons(x,_)) => ISome(x)
    case _ => INone()
  }
}

// pattern matching with if
def f(t: BTree): Int = {
  t match {
    case Leaf() => 0
    case Node(n,_,_) if(n<=10) =>1
    case Node(_,_,_) => 2
  }
}

// check whether x is in BTree
def find(t: BTree,x:Int): Boolean =
  t match {
    case Leaf() => false
    case Node(key,_,_) if(x==key) => true
    case Node(_,l,r) => find(l,x) || find(r,x)
  }

// tail recursive version

// generate BTree (every key is 0)
def genTree(v: Int,n: Int) : BTree = {
  @tailrec
  def genTreeIter(t: BTree,m : Int) : BTree = {
    m match {
      case 0 => t
      case _ => genTreeIter(Node(v, t, Leaf()), m - 1)
    }
  }
  genTreeIter(Leaf(),n)
}


sealed abstract class BTList
case class BTNil() extends BTList
case class BTCons(t:BTree,next: BTList) extends BTList

def findTail(t: BTree, x: Int): Boolean = {
  @tailrec
  def findIter(t: BTList,x: Int) : Boolean = {
    t match {
      case BTNil() => false
      case BTCons(Leaf(),next) => findIter(next,x)
      case BTCons(Node(key,l,r),next) =>
        if(x==key) true
        else findIter(BTCons(l,BTCons(r,next)),x)
    }
  }
  findIter(BTCons(t,BTNil()),x)
}

findTail(genTree(0,100000),1)


