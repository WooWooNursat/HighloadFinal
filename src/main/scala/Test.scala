import scala.collection.mutable
import scala.util.matching.Regex

object Test extends App{
  def test1 = {
    val pattern = new Regex("[\\*\\/\\-\\+]")
    val pattern2 = new Regex("\\d{1,}")
    val stack = mutable.Stack.empty[String]
    var varSign = ""

    var signs = pattern.findAllMatchIn("82/2").mkString(" ").split(' ').toList
    var nums = pattern2.findAllMatchIn("82/2").mkString(" ").split(' ').toList

    while (signs.nonEmpty) {
      stack.push(nums.head)
      nums = nums.tail

      signs.head match {
        case "*" =>
          stack.push((stack.pop().toDouble * nums.head.toDouble).toString)
          nums = nums.tail
        case "/" =>
          stack.push((stack.pop().toDouble / nums.head.toDouble).toString)
          nums = nums.tail
        case sign if sign == "-" || sign == "+" =>
          if (varSign.isEmpty) varSign = signs.head
          else {
            varSign match {
              case "+" => stack.push((stack.pop().toDouble + stack.pop().toDouble).toString)
              case "-" => stack.push((stack.pop().toDouble * (-1) + stack.pop().toDouble).toString)
            }
            varSign = signs.head
          }
      }
      signs = signs.tail

      if (signs.isEmpty) {
        if (nums.nonEmpty) stack.push(nums.head)
        varSign match {
          case "+" => stack.push((stack.pop().toDouble + stack.pop().toDouble).toString)
          case "-" => stack.push((stack.pop().toDouble * (-1) + stack.pop().toDouble).toString)
          case "" =>
        }
      }
    }
    val response = stack.pop().toDouble
    if (response / response.toInt != 1.0) println(response.toString)
    else println(response.toInt.toString)
    println(signs)
  }
  val pattern = new Regex("[\\/\\*\\-\\+\\(\\)]|[0-9]+")
  println(pattern.findAllMatchIn("31*6-3(222-111)+123").mkString(" ").split(' ').foreach(x => print(x + ", ")))

  var items = pattern.findAllMatchIn("31*6-3*(222-111)3+123").mkString(" ").split(' ').toList
  def check(items: List[String]): Boolean ={
    if (items.size == 1) return true
    items.head match {
      case sign if sign.equals("+") || sign.equals("-") || sign.equals("*") || sign.equals("/") || sign.equals("(") =>
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
  val bool = check(items)
  if(!bool) println("Wrong expression")
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

  println("items: " + items)
  println("queue: " + queue)
  println("signs: " + signs)
//
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
  if (response / response.toInt != 1.0) println(response.toString)
  else println(response.toInt.toString)
}
