package singletons

//import shapeless._
//import SingletonTypes._

// copied and pased from a source
sealed trait Nat
trait Z extends Nat
case object Z extends Z
case class S[N <: Nat](n: N) extends Nat


abstract class LTENat[A <: Nat, B <: Nat]
case class LTEZero[A <: Nat](x: A) extends LTENat[Z, A]
case class LTESucc[A <: Nat, B <: Nat](x: LTENat [A,B]) extends LTENat[S[A], S[B]]

case class MyException(message: String) extends Exception(message)



class Node[N <: Nat]( var value: N
                       , var next: Option[Node[_]]){


  def insert[M <: Nat](m: M, pf: LTENat[N, M]): Unit = {
    next match {
      case None => next = Some(new Node(m))
    }
  }



// def methods insert and delete



}


class LinkedList {
  val head: Option[Node[N]] = None
  val count: Int = 0

  // this below is wrong as written
  def insert(n: Nat): Unit = {
    count += 1
    head match {
      case None => head = Some(new Node(n, None))
      case Some(h) => h.insert(n)
    }
  }

  def delete(n: Nat): Unit = {
    count -= 1
    if (count == 0){
      head = None
    }else {
      head match {
        case None => throw MyException("error 1")
        case Some(h) => h.delete(n)
      }
    }
  }


}


object Main extends App{
  println("ok")
  val three: S[S[S[Z]]] = S(S(S(Z)))
  val zero: Z = Z
  val zeroLTthree: LTENat[Z, S[S[S[Z]]]] = LTEZero(three)
  val oneLTfour: LTENat[S[Z], S[S[S[S[Z]]]]]
    = LTESucc(zeroLTthree)

}


/*

Note to self.
 When ready, implement a ENat with a
  constraint on the nat (like it must be less than n)
  with context bounds

*/











