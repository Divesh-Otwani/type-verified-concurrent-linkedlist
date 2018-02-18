package ChatRoom


/*

In this file, I explain the tersely documented
example akka gives on "Akka Typed"

 The idea of the example is to implement a chatroom.
 There is a chatroom actor that holds a list of
 actorRefs called sessions that it can send out
 messages to.

 The chatroom can recieve `Command`s. It will
 accumulate a list of ActorRef[SessionEvent]'s.

 It will accept some `GetSession` requests and
 reject others. Once accepted, that actor will be notified
 and it can then send a PostMessage to the actorRef they
 were given. That, through some convertion thing
 is converted into a PostSessionMessage command
 that is sent to the chatroom actor and the chatroom
 actor just sends all it's sessions that message.


*/



import akka.typed._
import akka.typed.scaladsl.Actor._


/*

 Below, we completely define the interface in types.

*/

sealed trait Command
final case class
  GetSession(screenName: String, replyTo: ActorRef[SessionEvent])
  extends Command
sealed trait SessionEvent
final case class
  SessionGranted(handle: ActorRef[PostMessage])
    extends SessionEvent
final case class
  SessionDenied(reason: String) extends SessionEvent
final case class
  MessagePosted(screenName: String, message: String)
    extends SessionEvent

final case class PostMessage(message: String)


private final case class
  PostSessionMessage(screenName: String, message: String)
    extends Command

/*

 As I see it, Behavior[Interface] is a superclass
 of Stateless[Interface] and Stateful[Interface].

 The first argument is the context.

 The interface allows

 1) ctx.spawn(Behavior[Interface], String): ActorRef[Interface]
     to add an actor

2) ctx.spawnAdapter {p: SomeType => {expression that evaluates
 to something that actor can deal with, an inst of it's
 interface}} : ActorRef[SomeType]

 creates one actor child and returns an actorRef to it.
 When this child recieves messages, it converts them
 to a type that the current actor supports and forwards them.
 It is an adapter.

3) ctx.watch() which apparently watches for an actors's
 death and sends a Terminated(ActorRef) when it dies.
 I think (not sure) only an actor of type Behavior[akka.NotUsed]
 can watch actors.

*/


object Main extends App {


  def chatRoom
    (sessions: List[ActorRef[SessionEvent]] = List.empty)
      : Behavior[Command] =
    Stateful[Command] { (ctx, msg) =>
      msg match {
        case GetSession(screenName, client) =>
          val wrapper = ctx.spawnAdapter {
            p: PostMessage =>
              PostSessionMessage(screenName, p.message)
          }
          client ! SessionGranted(wrapper)
          chatRoom(client :: sessions)
          // this ^ was a recursive call
          // to return a new actor that
          // just has another session.
          // :: is Cons
        case PostSessionMessage(screenName, message) =>
          val mp = MessagePosted(screenName, message)
          sessions foreach (_ ! mp)
          Same
      }
    }

/*

 As I understand it, an Stateful[Interface] actor
 is constructed just like a Stateless[Interface] actor,
 by using the name as a constructor:
   Stateless[Interface] {
     (ctx, msg) => {x: Stateful[Interface]}
   }

   except now, the return value of the `recieve` is a new
   actor of the same type/interface. Further,
   Stateless actors die when the system is shut down.
   Here you can stop actors; see below.


   For this, some syntax is given:
     `Same` means the actor remains
     `Stopped` means the actor is going to die now
      without processing any more messages.

      `Unhandled` means what you think it means.


*/


  val gabbler =
    Stateful[SessionEvent] { (_, msg) =>
      msg match {
        case SessionDenied(reason) =>
          println(s"cannot start chat room session: $reason")
          Stopped
        case SessionGranted(handle) =>
          handle ! PostMessage("Hello World!")
          Same
        case MessagePosted(screenName, message) =>
          println(s"msg posted by '$screenName': $message")
          Stopped
      }
    }

  // akka.NotUsed means no messages can be sent to this.
  // Rather, we use something like the prestart behavior
  // to get the whole load of work going.
  val main: Behavior[akka.NotUsed] =
    Stateful(
      behavior = (_, _) => Unhandled,
      signal = { (ctx, sig) =>
      sig match {
        case PreStart =>
          val chatRoomref = ctx.spawn(chatRoom(), "chatroom")
          val gabblerRef = ctx.spawn(gabbler, "gabbler")
          ctx.watch(gabblerRef)
          chatRoomref ! GetSession("ol’ Gabbler", gabblerRef)
          Same
        case Terminated(ref) =>
          // we get this when gabblerRef dies because
          // we are watching it
          Stopped // in this case, we kill the system
        case _ =>
          Unhandled
      }
    })


  val system = ActorSystem("ChatRoomDemo", main)

}









