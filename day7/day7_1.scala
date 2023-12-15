import scala.io.Source
import scala.util.matching.Regex

object DaySeven {
    def main(args: Array[String]): Unit ={
        val test_case = readLines("day7/data.txt")
        val res = getHand(test_case(0))
        println(res)
    }
}

def readLines(path:String): List[String] = {
    Source.fromFile(path).getLines.toList
}

def getHand(s: String): List[Int] = {
    // Hand
    val pattern:Regex = """[A-Z0-9]{5}""".r
    val m = pattern.findFirstIn(s).get
    val l = m.split("").toList
    println(l)
    val hand = l.flatMap( str => cardMap.get(str))
    
    
}

val cardMap: Map[String, Int] = Map(
    "2"->2, 
    "3"->3, 
    "4"->4,
    "5"->5,
    "6"->6,
    "7"->7,
    "8"->8,
    "9"->9,
    "T"->10,
    "J"->11,
    "Q"->12,
    "K"->13,
    "A"->14
)