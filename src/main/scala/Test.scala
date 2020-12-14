import scala.collection.mutable
import scala.util.matching.Regex

object Test extends App{
  val pattern = new Regex("[\\*\\/\\-\\+]")
  val pattern2 = new Regex("\\d{1,}")
  val stack = mutable.Stack.empty[String]
  var varSign = ""

  var signs = pattern.findAllMatchIn("8/2").mkString(" ").split(' ').toList
  var nums = pattern2.findAllMatchIn("8/2").mkString(" ").split(' ').toList

  while(signs.nonEmpty) {
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
        if(varSign.isEmpty) varSign = signs.head
        else {
          varSign match {
            case "+" => stack.push((stack.pop().toDouble + stack.pop().toDouble).toString)
            case "-" => stack.push((stack.pop().toDouble*(-1) + stack.pop().toDouble).toString)
          }
          varSign = signs.head
        }
    }
    signs = signs.tail

    if(signs.isEmpty) {
      if(nums.nonEmpty) stack.push(nums.head)
      varSign match {
        case "+" => stack.push((stack.pop().toDouble + stack.pop().toDouble).toString)
        case "-" => stack.push((stack.pop().toDouble*(-1) + stack.pop().toDouble).toString)
        case "" =>
      }
    }
  }
  val response = stack.pop().toDouble
  if(response/response.toInt != 1.0) println(response.toString)
  else println(response.toInt.toString)
  println(signs)
}
