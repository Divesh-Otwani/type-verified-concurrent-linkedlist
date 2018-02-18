package Test

import shapeless._
import syntax.typeable._

case class MyException(message: String) extends Exception(message)


abstract sealed trait Nat
abstract sealed trait Z extends Nat
case object Z extends Z
case class S[N <: Nat](n: N) extends Nat


abstract class LTENat[A <: Nat, B <: Nat]
    extends Typeable[LTENat[A,B]]
case class LTEZero[A <: Nat](x: A) extends LTENat[Z, A]
case class LTESucc[A <: Nat, B <: Nat](x: LTENat [A, B])
    extends LTENat[S[A], S[B]]


object Proofs  extends App {


  def queryLT[A <: Nat, B <: Nat](n:A, m: B): Option[LTENat[A,B]] = {
    (n,m) match {
      case (Z, _) =>
        LTEZero[B] match {case r: LTENat[A,B] => Some(r)}
      case (_, Z) => None
      case (S(x), S(y)) =>
        queryLT(x,y) match {
          case Some(pf) =>
            LTESucc(pf) match {case r:LTENat[A,B] => Some(r)}
          case None => None
        }
    }
  }


}


