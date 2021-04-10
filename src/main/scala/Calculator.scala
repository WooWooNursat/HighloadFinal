import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}

import scala.collection.mutable
import scala.util.matching.Regex



object Calculator {
  final case class Calculate(exp: String, replyTo: ActorRef[String])

  def apply(): Behavior[Calculate] = {
    Behaviors.setup { context =>

      Behaviors.receiveMessage {
        case Calculate(exp, replyTo) =>
          replyTo ! calculate(exp)
          Behaviors.same
      }
    }
  }

  def calculate(exp: String): String ={
    val pattern = new Regex("[\\/\\*\\-\\+\\(\\)]|[0-9]+")

    var items = pattern.findAllMatchIn(exp).mkString(" ").split(' ').toList
    if(!check(items)) return "Wrong Expression"
    val signs = mutable.Stack.empty[String]
    val queue = mutable.Queue.empty[String]
    val result = mutable.Stack.empty[String]

    while(items.nonEmpty) {
      items.head match {
        case "(" => signs.push(items.head)
        case sign if sign.equals("+") || sign.equals("-") =>
          if(signs.nonEmpty) {
            if (!signs.top.equals("(")) {
              queue.enqueue(signs.pop())
            }
          }
          signs.push(items.head)
        case sign if sign.equals("*") || sign.equals("/") =>
          if(signs.nonEmpty) {
            if (signs.top.equals("*") || signs.top.equals("/")) {
              queue.enqueue(signs.pop())
            }
          }
          signs.push(items.head)
        case ")" =>
          while (!signs.top.equals("(")) {
            queue.enqueue(signs.pop())
          }
          signs.pop()
        case _ => queue.enqueue(items.head)
      }
      items = items.tail
    }
    while(signs.nonEmpty) queue.enqueue(signs.pop())

    while(queue.nonEmpty) {
      queue.front match {
        case "*" => result.push((result.pop().toDouble * result.pop().toDouble).toString)
        case "/" =>
          val r = result.pop
          result.push((result.pop().toDouble / r.toDouble).toString)
        case "+" => result.push((result.pop().toDouble + result.pop().toDouble).toString)
        case "-" => result.push((result.pop().toDouble*(-1) + result.pop().toDouble).toString)
        case _ => result.push(queue.front)
      }
      queue.dequeue()
    }

    val response = result.pop().toDouble
    if (response / response.toInt != 1.0) response.toString
    else response.toInt.toString
  }

  def check(items: List[String]): Boolean ={
    if (items.size == 1) return true
    items.head match {
      case sign if sign.equals("+") || sign.equals("-") || sign.equals("*") || sign.equals("/") =>
        items.tail.head match {
          case sign if sign.equals("+") || sign.equals("-") || sign.equals("*") || sign.equals("/") || sign.equals(")") =>
            return false
          case _ =>
        }
      case ")" => items.tail.head match {
        case sign if !sign.equals("+") || !sign.equals("-") || !sign.equals("*") || !sign.equals("/") || sign.equals(")")
          || sign.equals("(")=>
          return false
        case _ =>
      }
      case "(" => items.tail.head match {
        case sign if sign.equals("+") || sign.equals("-") || sign.equals("*") || sign.equals("/") || sign.equals(")")
          || sign.equals("(")=>
          return false
        case _ =>
      }
      case _ => if(items.tail.head.equals("(")) return false
    }

    check(items.tail)
  }
}
