package TypedActors

// next 4 lines copied and pased from a source
sealed trait Nat
trait Z extends Nat
case object Z extends Z
case class S[N <: Nat](n: N) extends Nat


abstract class LTENat[A <: Nat, B <: Nat]
case class LTEZero[A <: Nat](x: A) extends LTENat[Z, A]
case class LTESucc[A <: Nat, B <: Nat](x: LTENat [A,B])
    extends LTENat[S[A], S[B]]

case class MyException(message: String) extends Exception(message)









