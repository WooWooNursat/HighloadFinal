import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import akka.util.Timeout

import scala.concurrent.duration.DurationInt

object Main extends App{
  val rootBehavior = Behaviors.setup[Node.Command] { context =>

    implicit val system = context.system
    implicit val ex = context.executionContext
    implicit val scheduler = system.scheduler
    implicit val timeout = Timeout(5.seconds)

    val node = context.spawnAnonymous(Node())
    val router = new NodeRouter(node)(system, ex)

    Server.startHttpServer(router.route, "localhost", 8080)(system, ex)
    Behaviors.empty
  }
  val system = ActorSystem[Node.Command](rootBehavior, "Calculator")

}
