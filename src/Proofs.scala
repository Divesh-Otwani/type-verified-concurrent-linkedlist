package TypedActors

object Proofs {
  // Here, I'll put cool proofs and functions I
  // need to verify the correctness.

  def queryLT[A <: Nat, B <: Nat]
    (n:A, m: B): Either[LTENat[A,B], LTENat[B,A]] = {
    (n,m) match {
      case (Z, _) =>
        Left(LTEZero[B].asInstanceOf[LTENat[A,B]])
      case (_, Z) =>
        Right(LTEZero[A].asInstanceOf[LTENat[B,A]])
      case (S(x), S(y)) =>
        queryLT(x,y) match {
          case Left(pf) =>
            Left(LTESucc(pf).asInstanceOf[LTENat[A,B]])
          case Right(pf) =>
            Right(LTESucc(pf).asInstanceOf[LTENat[B,A]])
        }
    }
  }

  def lteTrans[A <: Nat, B <: Nat, C <: Nat](
    x: LTENat[A,B],
    y: LTENat[B,C]
  ): LTENat[A,C] = {
    (x,y) match {
      case (LTEZero(), _) =>
        LTEZero[C].asInstanceOf[LTENat[A,C]]
      case (_, LTEZero()) =>
        LTEZero[C].asInstanceOf[LTENat[A,C]]
      case (LTESucc(a), LTESucc(b)) =>
        LTESucc(lteTrans(a,b)).asInstanceOf[LTENat[A,C]]
    }
  }



}

