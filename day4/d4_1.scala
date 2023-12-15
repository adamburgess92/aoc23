import scala.io.Source
import scala.util.matching.Regex

object Main {
    def main(args: Array[String]): Unit = {
        // val test_case = "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
        // val draw = getDraw(test_case)
        // val winners = getWinners(test_case)
        // val res = calcWin(winners, draw)

        val in = readLines("day4/data.txt")
        val res = processAllLines(in)
        println(res)
    }
}

def readLines(path:String): List[String] = {
    Source.fromFile(path).getLines.toList
}

def getWinners(input: String): List[Int] = {
    // Extract string of digits between : and | 
    val pattern: Regex = """:(.*?)\|""".r
    val s = pattern.findFirstMatchIn(input).map(_.group(1))
    // Split string into list of Ints:
    val winners: List[Int] = s.getOrElse("").trim.split("\\s+").map(_.toInt).toList
    winners
}

def getDraw(input: String): List[Int] = {
    // extract string of digits between | and end:
    val pattern: Regex = """\|(.*)$""".r
    val s = pattern.findFirstMatchIn(input).map(_.group(1))
    // Split string into list of ints:
    val draw: List[Int] = s.getOrElse("").trim.split("\\s+").map(_.toInt).toList
    draw
}

def calcWin(winners:List[Int], draw:List[Int]): Int = {
    var a:Int = 0
    for (d <- draw) {
        for (w <- winners) {
            if (d==w && a==0){
                a +=1
            } else if (d==w && a>0) {
                a = a*2
            }
        }
    }
    a
}

def processLine(input:String): Int = {
    val winners = getWinners(input)
    val draw = getDraw(input)
    val win = calcWin(winners, draw)
    win
}

def processAllLines(input:List[String]): Int = {
    def inner(input:List[String], a:Int): Int = input match {
        case h::t => inner(t, a+processLine(h))
        case Nil => a
    }
    val a:Int = 0
    inner(input, a)
}