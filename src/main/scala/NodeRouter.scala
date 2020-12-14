import akka.actor.typed.scaladsl.AskPattern.Askable
import akka.actor.typed.{ActorRef, ActorSystem}
import akka.http.scaladsl.model.{HttpHeader, StatusCodes}
import akka.http.scaladsl.server.{Directives, Route}
import akka.util.Timeout
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport._
import io.circe.generic.auto._

import scala.concurrent.duration.DurationInt
import scala.concurrent.{ExecutionContext, Future}

trait Router {
  def route: Route
}

class NodeRouter(node:ActorRef[Node.Command])(implicit system: ActorSystem[_], ex:ExecutionContext)
  extends  Router
    with  Directives {

  implicit def scheduler = system.scheduler

  implicit lazy val timeout = Timeout(5.seconds)

  override def route = {
    concat(
      expRoutes,
      historyRoutes
    )
  }

  lazy val expRoutes: Route = pathPrefix("calculate") {
    post {
      headerValueByName("token") { token =>
        entity(as[Expression]) { expression =>
          val calcFuture: Future[Calc] = node.ask(ref => Node.Calculate(token, expression.expression, ref)).mapTo[Calc]
          onSuccess(calcFuture) { response =>
            complete(StatusCodes.OK, response)
          }
        }
      }
    }
  }

  lazy val historyRoutes: Route = pathPrefix("history") {
    get {
      headerValueByName("token") { token =>
        val historyFuture: Future[History] = node.ask(Node.GetHistory(
          token,
          _)).mapTo[History]
        onSuccess(historyFuture) { history =>
          complete(StatusCodes.OK, history)
        }
      }
    }
  }
}