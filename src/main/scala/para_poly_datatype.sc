import scala.annotation.tailrec

sealed abstract class MyOption[A]
case class MyNone[A]() extends MyOption[A]
case class MySome[A](x:A) extends MyOption[A]

sealed abstract class MyList[A]
case class MyNil[A]() extends MyList[A]
case class MyCons[A](x:A,next:MyList[A]) extends MyList[A]

sealed abstract class BTree[A]
case class MyLeaf[A]() extends BTree[A]
case class MyNode[A](value:A,l:BTree[A],r:BTree[A]) extends BTree[A]

//sealed abstract class BSTree[A]
//case class Leaf[A]() extends BSTree[A]
//case class Node[A](key:Int,value:A,l:BSTree[A],r:BSTree[A]) extends BSTree[A]
//
//@tailrec
//def lookUp[A](t: BSTree[A],key:Int) : MyOption[A] =
//  t match {
//    case Leaf() => MyNone[A]()
//    case Node(x,v,l,r) =>
//      x match {
//        case _ if x==key => MySome[A](v)
//        case _ if x>key => lookUp(l,key)
//        case _ => lookUp(r,key)
//      }
//  }

// improved version
type BSTree[A] = BTree[(Int,A)]

@tailrec
def lookup[A](t: BSTree[A], k: Int): MyOption[A] =
  t match {
    case MyLeaf() => MyNone()
    case MyNode((key,value),l,r) =>
      key match {
        case _ if k==key => MySome(value)
        case _ if k>key => lookup(r,k)
        case _ if k<key => lookup(l,k)
      }
  }

