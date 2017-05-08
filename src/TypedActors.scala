package TypedActors

import akka.typed._
import akka.typed.scaladsl.Actor._
import scala.collection.mutable.ArrayBuffer

// not sure if I need this:
import scala.concurrent.ExecutionContext.Implicits.global


// next 4 lines copied and pasted from a source
sealed trait Nat
sealed trait Z extends Nat
case object Z extends Z
case class S[N <: Nat](n: N) extends Nat

case class ExNat(n: N forSome {type N <: Nat})

abstract class LTENat[A <: Nat, B <: Nat]
case class LTEZero[A <: Nat]() extends LTENat[Z, A]
case class LTESucc[A <: Nat, B <: Nat](x: LTENat[A, B])
    extends LTENat[S[A], S[B]]


case class MyException(message: String) extends Exception(message)



/*

 The interface:

  Linked Lists accept 3 messages and
  holds an Option[HeadNode[N]] and len.

 1) Insert:
       If no head, add one. Otherwise...
       Inc len. Make a fake node with 0,
       make it an adapter and sent it along.

          Q: will this mess up the node we sent the
          message to if the type of the LL
          changes?

 2) Delete:
      If the len is 1, delete the head.
      Otherwise pass it along.

 3) PrintList: If head, pass it along. No args.

 4) ChangeHead: Just changes head. No proofs.


  Nodes accept 5 messages and
  holds nats and optional next pointers.

 1) Insert
 2) Delete
     note: nodes don;t die.
     This is because I can't safely deal with deadletters.
     In general, this is a problem with sending refs in messages.
     In a double LL, this would not be a problem.
 3) ChangeNext
 4) PrintList: arg of partial string.


 */



sealed trait NodeMsg[N <: Nat] // node is val indexed
case class Insert[A <: Nat, N <: Nat, P <: Nat]
  (ins: A, prev: ActorRef[NodeMsg[P]], insGTEprev: LTENat[P,A])
    extends NodeMsg[N]
case class Delete[A <: Nat, N <: Nat, P <: Nat]
  (del: A, prev: ActorRef[NodeMsg[P]], prevLT: LTENat[P,N])
    extends NodeMsg[N]
// N is curr, I is to insert and S is the one after N.
case class ChangeNext[N <: Nat, I <: Nat, X <: Nat]
  ( newnext: Option[Next[N,I]]
  , sender: Option[ActorRef[NodeMsg[X]]] = None)
    extends NodeMsg[N]
case class PrintList[N <: Nat](s: String = "") extends NodeMsg[N]
case class ExitHibernation[N <: Nat]() extends NodeMsg[N]


sealed trait LLMsg
case class InsertLL[N <: Nat](i: N) extends LLMsg
case class DeleteLL[N <: Nat](i: N) extends LLMsg
case class PrintLL() extends LLMsg
case class ChangeHead[N <: Nat](newhead: Option[ActorRef[NodeMsg[N]]])
  extends LLMsg



// Helper classes:

// N is the nat of the current node holding that pointer.
// I is the nat of the node being inserted
case class Next[N <: Nat, I <: Nat]
  (next: ActorRef[NodeMsg[I]], pf: LTENat[N,I])
//class Head[N <: Nat](x: N, head: ActorRef[NodeMsg[N]])

sealed trait NodeState[N <: Nat]
case class Inactive[P <: Nat, N <: Nat]
  (parent: ActorRef[NodeMsg[P]], parLTE: LTENat[P, N])
    extends NodeState[N]
case class Active[N <: Nat]() extends NodeState[N]
case class Hibernating[N <: Nat]() extends NodeState[N]



object Main extends App {

  def makeNode[N <: Nat, A <: Nat]
    ( n: N
    , next: Option[Next[N, A]]
    , ll: ActorRef[LLMsg]
    , state: NodeState[N] = Active[N]()
    ): Behavior[NodeMsg[N]] = {

    Stateful[NodeMsg[N]] { (ctx, msg) =>
      //println(s"Node $n, mode $state got msg $msg")
      state match {
        case Active() =>
          msg match {
            case Insert(ins, prev, insGTEprev) =>
              //println(s"Inserting $ins at node $n")
              Proofs.queryLT(n, ins) match {
                case Right(pf) =>
                  val nnext = new Next(ctx.self, pf)
                  val nnode = makeNode(ins, Some(nnext), ll)
                  val nref = ctx.spawn(nnode, UniqueNames.name())
                  val newnext = new Next(nref, insGTEprev)
                  prev ! ChangeNext(Some(newnext), Some(ctx.self))
                  makeNode(n,next,ll, Hibernating())
                case Left(pf) =>
                  next match {
                    case None =>
                      val nnode = makeNode(ins, None, ll)
                      val nref = ctx.spawn(nnode, UniqueNames.name())
                      // Do we need unique names?
                      val nxt = new Next(nref, pf)
                      makeNode(n, Some(nxt), ll)
                    case Some(Next(nref, npf)) =>
                      nref ! Insert(ins, ctx.self, pf)
                      Same
                  }
              }
            case ChangeNext(newnext, ref) =>
              for {r <- ref} yield {r ! ExitHibernation()}
              makeNode(n, newnext, ll)
            case Delete(del, prev, prevLT) =>
              if(del == n){
                next match {
                  case None =>
                    prev ! ChangeNext(None, None)
                    makeNode(n, next, ll, Inactive(prev, prevLT))
                  case Some(Next(nref, npf)) =>
                    val newpf = Proofs.lteTrans(prevLT, npf)
                    val nnext = new Next(nref, newpf)
                    prev ! ChangeNext(Some(nnext), None)
                    makeNode(n, next, ll, Inactive(prev, prevLT))
                }
              }else{
                for {nxt <- next}
                yield {nxt.next ! Delete(del, ctx.self, nxt.pf)}
                Same
              }
            case PrintList(s) =>
              val Ts: String = s ++ " " ++ toStr(n)
              next match {
                case None =>
                  println(s"List: $Ts")
                  Same
                case Some(Next(nref, npf)) =>
                  nref ! PrintList(Ts)
                  Same
              }
            case _ => throw MyException("error1")
          }
        case Inactive(parent, parLTE) =>
          msg match {
            case PrintList(s) =>
              next match {
                case None =>
                  println(s"List $s")
                  Same
                case Some(Next(nref, npf)) =>
                  nref ! PrintList(s)
                  Same
              }
            case Insert(ins, _, _) =>
              ll ! InsertLL(ins)
              Same
            case Delete(del, prev, prevLT) =>
              for {nxt <- next}
              yield {
                val newpf = Proofs.lteTrans(prevLT, nxt.pf)
                nxt.next ! Delete(del, prev, newpf)
              }
              Same
            case ChangeNext(newnext, ref) =>
              newnext match {
                case None =>
                  parent ! ChangeNext(None, ref)
                  Same
                case Some(Next(nref, npf)) =>
                  val newpf = Proofs.lteTrans(parLTE, npf)
                  val nnext = Some(Next(nref, newpf))
                  parent ! ChangeNext(nnext, ref)
                  Same
              }
            case _ => throw MyException("error2")
          }
        case Hibernating() =>
          msg match {
            case Insert(ins, _, _) =>
              ll ! InsertLL(ins)
              Same
            case Delete(del, _, _) =>
              ll ! DeleteLL(del)
              Same
            case ExitHibernation() =>
              makeNode(n, next, ll, Active())
            case PrintList(_) =>
              ll ! PrintLL()
              Same
            case ChangeNext(newnext, ref) =>
              for {r <- ref} yield {r ! ExitHibernation()}
              makeNode(n, newnext, ll, Hibernating())
          }
      }

    }

  }


  def linkedList[N <: Nat]
    (
    head: Option[ActorRef[NodeMsg[N]]] = None
    ): Behavior[LLMsg] = {
    Stateful[LLMsg] { (ctx, msg) =>
      //println(s"LL got msg $msg")
      msg match {
        case ChangeHead(newhead) =>
          linkedList(newhead)
        case PrintLL() =>
          head match {
            case None =>
              println("Empty Linked List")
              Same
            case Some(ref) =>
              ref ! PrintList()
              Same
          }
        case DeleteLL(i) =>
          val fake: ActorRef[NodeMsg[Z]] =
            ctx.spawnAdapter {
              case Delete(del, _, _) => DeleteLL(del)
              case ChangeNext(nxt, ref) =>
                //for {r <- ref} yield {r ! ExitHibernation()}
                nxt match {
                  case None => ChangeHead(None)
                  case Some(Next(ref, _)) =>
                    ChangeHead(Some(ref))
                }
              case _ => throw MyException("error3")
            }
          for {hd <- head}
          yield {
            hd ! Delete( i
                       , fake
                       , LTEZero() // will this work???
                       )
          }
          Same
        case InsertLL(i) =>
          head match {
            case None =>
              val newhead = makeNode(i, None, ctx.self)
              val headref = ctx.spawn(newhead, UniqueNames.name())
              linkedList(Some(headref))
            case Some(hd) =>
              val fake: ActorRef[NodeMsg[Z]] =
                ctx.spawnAdapter {
                  case Insert(ins, _, _) => InsertLL(ins)
                  case ChangeNext(next, ref) =>
                    for {r <- ref} yield {r ! ExitHibernation()}
                    next match {
                      case None =>
                        throw MyException("Inserts can't delete.")
                      case Some(Next(ref, _)) =>
                        ChangeHead(Some(ref))
                    }
                  case _ =>
                    throw MyException("error4")
                }
              hd ! Insert( i
                         , fake
                         , LTEZero().asInstanceOf[LTENat[Z, Nat]]
                         )
              linkedList(head)
          }

      }
    }
  }


  val main: Behavior[akka.NotUsed] =
    Stateful(
      behavior = (_, _) => Unhandled,
      signal = { (ctx, sig) =>
        sig match {
          case PreStart =>
            val llref = ctx.spawn(linkedList(), "theLL")
            val numops = 100
            val numtds = 3
            var threads: ArrayBuffer[Thread] = ArrayBuffer()
            for (i <- 1 to numtds){
              threads += new Tester(llref, numops)
            }
            for (td <- threads){
              td.start()
            }
            for (td <- threads){
              td.join()
            }
            Same
          case _ => Same
        }
      }
    )

  val system = ActorSystem("system", main)

  def toInt[N <: Nat](x: N): Int = {
    x match {
      case Z => 0
      case S(y) => 1 + toInt(y)
    }
  }

  def toStr[N <: Nat](x:Nat): String = toInt(x).toString()

  def toNat(i: Int): ExNat = {
    if (i < 0){
      throw MyException("Not a nat.")
    }else if (i==0){
      ExNat(Z)
    }else{
      toNat(i-1) match {
        case ExNat(n) => ExNat(S(n))
      }
    }
  }

}

class Tester(llref: ActorRef[LLMsg], numops: Int) extends Thread{
  override def run(){
    for (i <- 0 to numops){
      var decide = scala.util.Random.nextInt(101)
      var randNat =
        Main.toNat(scala.util.Random.nextInt(201)) match {
          case ExNat(n) => n
        }
      if (decide <= 8){
        llref ! PrintLL()
      }else if(decide <= 50){
        llref ! DeleteLL(randNat)
      }else{
        llref ! InsertLL(randNat)
      }
    }
  }
}


object UniqueNames {
  var i: Integer = 0
  def name(): String = {
    i.synchronized {
      val r = i
      i += 1
      return r.toString()
    }
  }
}

/* Testing

            llref ! InsertLL(S(S(S(Z))))
            llref ! InsertLL(S(S(Z)))
            llref ! InsertLL(S(Z))
            Thread.sleep(300)
            llref ! PrintLL()
            Thread.sleep(300)
            llref ! DeleteLL(S(S(Z)))
            Thread.sleep(300)
            llref ! PrintLL()
            Thread.sleep(300)
            // insert 5,7,6,4,5,8
            llref ! InsertLL(S(S(S(S(S(Z))))))
            llref ! InsertLL(S(S(S(S(S(S(S(Z))))))))
            llref ! InsertLL(S(S(S(S(S(S(Z)))))))
            llref ! InsertLL(S(S(S(S(Z)))))
            llref ! InsertLL(S(S(S(S(S(Z))))))
            llref ! InsertLL(S(S(S(S(S(S(S(S(Z)))))))))
            Thread.sleep(2000)
            llref ! PrintLL()
            Thread.sleep(400)
            llref ! DeleteLL(S(S(S(S(S(S(S(S(Z)))))))))
            llref ! DeleteLL(S(S(S(S(S(Z))))))
            llref ! DeleteLL(S(S(S(S(S(Z))))))
            Thread.sleep(1000)
            llref ! PrintLL() // 1 3 4 6 7
            Thread.sleep(500)
            llref ! DeleteLL(S(Z))
            Thread.sleep(500)
            llref ! PrintLL() // 3 4 6 7
            Thread.sleep(500)
            llref ! InsertLL(Z)
            Thread.sleep(500)
            llref ! PrintLL() // 0 3 4 6 7




 */




  // Testing
  /*
  println("Hi world. Testing here.")
  val three: S[S[S[Z]]] = S(S(S(Z)))
  val one: S[Z] = S(Z)
  val six: S[S[S[S[S[S[Z]]]]]]
      = S(S(S(S(S(S(Z))))))
  val test1 = Proofs.queryLT(one, three) //left
  val test2 = Proofs.queryLT(three, three) //left
  val test3 = Proofs.queryLT(three,one) //right
  val test4 = Proofs.queryLT(three, six) // right
  println(s"We got $test1 ...")
  println(s"We got $test2 ...")
  println(s"We got $test3 ...")
  println(s"We got $test4 ...")
  //val trans: Option[LTENat[S[Z], S[S[S[S[S[S[Z]]]]]]]] =
    //for {t1 <- test; t2 <- test4}
    //yield {Proofs.lteTrans(t1,t2)}
    //println(s"1 <= 3 & 3 <= 6 => 1 <= 6 : $trans")
   */











