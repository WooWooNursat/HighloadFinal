import akka.actor.typed.scaladsl.AskPattern.Askable
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import akka.util.Timeout

import scala.concurrent.duration.DurationInt
import scala.util.{Failure, Success}

object Node {

  sealed trait Command
  final case class Calculate(token: String, expression: String, replyTo: ActorRef[Calc]) extends Command
  final case class GetHistory(token: String, replyTo: ActorRef[History]) extends Command


  def apply(): Behavior[Command] = {
    Behaviors.setup { context =>
      implicit def system: ActorSystem[Nothing] = context.system

      implicit val ex = system.executionContext

      implicit def scheduler = context.system.scheduler

      implicit lazy val timeout = Timeout(5.seconds)

      val historyService = new HistoryService()(system, ex)
      val calculatorService = new CalculatorService()(system, ex)
      val calc = context.spawnAnonymous(Calculator())

      Behaviors.receiveMessage {
        case Calculate(token, expression, replyTo) =>
          val ask = calc.ask(Calculator.Calculate(expression, _))
          ask.onComplete {
            case Success(value) =>
              replyTo ! Calc(expression, value)
              calculatorService.send(token, expression, value)
                .onComplete {
                  case Success(done) => println(done)
                  case Failure(exception) => println(exception)
                }
            case Failure(exception) => println(exception)
          }
          Behaviors.same

        case GetHistory(token, replyTo) =>
          historyService.control(token)
          Thread.sleep(2000)
          replyTo ! historyService.done()
          Behaviors.same

      }
    }
  }
}
