# CS356Lab8

This is a private repo for my lab 8 final project. 
Essentialy, I implement a singly linked list
with some (not a lot) typed verification
to ensure that the list remains ordered.


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


## Presentation Outline

- State the interface for typed actors: the type of the messages.
Quickly walk through the example.
- State the general design and show the LTE Gadt.
- Show the interface for message sending and note the significance.
- Mention where you cast and why that means if we don't error at runtime, we are correct.
- Demo with many threads.
- Walk through the insert method if time.



## Sources

* See here for [type level things and Nat class def](https://brianmckenna.org/blog/evenodd_agda_idris_haskell_scala)
* Some help from Alexander Konovalov @alexknvl on the scala/scala forum at https://gitter.im/scala/scala?source=orgpage on deciding how to deal with my GADT. We conluded that it was best to cast since errors would be caught at runtime anyway. Some other people helped too, but it was mainly this person.
* [This](http://www.drmaciver.com/2008/03/existential-types-in-scala/) for existential types.


Helpful in background understanding:

[Link](http://typelevel.org/blog/2014/07/06/singleton_instance_trick_unsafe.html)




