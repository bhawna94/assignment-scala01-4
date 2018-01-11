import org.apache.log4j.Logger

class Operations {


  def integerOperate(f: (Int, Int) => Int, number1: Int, number2: Int): Int = {

    f(number1, number2)
  }

 def sumOfList(list: List[Int]): Int = {

    def innerSum(sum: Int, list: List[Int]): Int = {
      list match {
        case head :: tail => innerSum(sum + head, tail)
        case head :: Nil => sum + head
      }
    }

    innerSum(0, list)
  }


    def maxList(list: List[Int]): Int = {

      list match {
        case head :: head2 :: tail if (head >= head2) => maxList(head :: tail)
        case head :: head2 :: tail if (head <= head2) => maxList(head2 :: tail)
        case head :: Nil => head
      }
    }
  }


object Operations extends App {

  val log = Logger.getLogger(this.getClass)
  val choice: String = "square"
  val object1 = new Operations
  val number1 = 10
  val number2 = 20
  val l1 = 1
  val l2 = 3
  val l3 = 5
  val l4 = 10
  val list1 = List(l1,l2,l3,l4)
  choice match {
    case "square" => log.info(object1.integerOperate((a, b) => (a * a + b * b), number1, number2))
    case "cube" => log.info(object1.integerOperate((a, b) => (a * a * a + b * b * b), number1, number2))
    case "sum" => log.info(object1.integerOperate((a, b) => (a + b), number1, number2))
  }
  log.info(object1.maxList(list1))
  log.info(object1.maxList(list1))

}
