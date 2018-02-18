# CS356Lab8

This is my final project for my Concurrency class.

I implement a concurrent ordered singly linked list
with typed verification
to ensure that the list remains ordered.

**You can concurrently delete and insert elements 
and the list remains ordered.**

I used typed actors ([see here](https://doc.akka.io/docs/akka/2.5/typed-actors.html)) and some cool
curry-howard isomorphism stuff to get things done.

There is a tiny part in /src/Proofs.scala where Scala's type system 
doesn't do what I'd like and I needed to use type coercion at that point.



## The Typed Interface


```scala
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
/*
These last 4 lines allow us to build a proof
of any less than relationship. Each object of some type
corresponds to a proof of a less than relationship 
(the curry howard correspondence, informally).
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

```


## Design

* Each node is an actor, and we have a Linked list actor
* Nodes can be active, inactive or hibernating.
* In order to force constraints by the LTE GADT, we need to pass specific kinds of messages. So, we index the message type and the thigns messages have to help this out. Main idea: when you hold GADTs as proofs, you should index your other types so you can easily construct those proofs
* We cast in two places. First, we use casts in the Proofs.scala file for querying LTE's and transitivity. Second, we use it once to cast a LTEZero when making a fake adapter actor for the Linked List actor.



## Sources

* See here for [type level things and Nat class def](https://brianmckenna.org/blog/evenodd_agda_idris_haskell_scala)
* Some help from Alexander Konovalov @alexknvl on the scala/scala forum at https://gitter.im/scala/scala?source=orgpage on deciding how to deal with my GADT. We conluded that it was best to cast since errors would be caught at runtime anyway. Some other people helped too, but it was mainly this person.
* [This](http://www.drmaciver.com/2008/03/existential-types-in-scala/) for existential types.


Helpful in background understanding:

[Link](http://typelevel.org/blog/2014/07/06/singleton_instance_trick_unsafe.html)




