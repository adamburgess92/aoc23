import scala.io.Source
import scala.util.matching.Regex

object DayTwo {
    def main(args: Array[String]): Unit = {
        // val data = readFile("day2/test_data.txt")
        val data = readFile("day2/data.txt")
        val res = sumIds(data)
        println(res)        
    }
}

def readFile(path:String): List[String] = {
    val source = Source.fromFile(path)
    val lines = try source.getLines().toList
    finally source.close()
    lines
}

def isGamePossible(input: String): (Int, List[Boolean]) = {
    val splitStr = input.split("[;|:]")
    // splitStr.foreach(println)

    val maxRed = 12
    val maxGreen = 13
    val maxBlue = 14

    val regGameNo: Regex = """Game (\d*)""".r
    val regRed: Regex = """(\d*)\sred""".r
    val regGreen: Regex = """(\d*)\sgreen""".r
    val regBlue: Regex = """(\d*)\sblue""".r
    
    val gameNo = regGameNo.findFirstMatchIn(splitStr(0)).map(_.group(1)).getOrElse("0").toInt
    
    val games = splitStr.slice(1, splitStr.length).toList

    def inner(ls: List[String], a:List[Boolean]): List[Boolean] = ls match {
        case h::t => {
            val numRed = regRed.findFirstMatchIn(h).map(_.group(1)).getOrElse("0").toInt
            val numGreen = regGreen.findFirstMatchIn(h).map(_.group(1)).getOrElse("0").toInt
            val numBlue = regBlue.findFirstMatchIn(h).map(_.group(1)).getOrElse("0").toInt

            // println(s"Red: $numRed")
            // println(s"Green: $numGreen")
            // println(s"Blue: $numBlue")

            if (numRed<=maxRed && numGreen<=maxGreen && numBlue<=maxBlue) {
                inner(t, a:+true)
            } else {
                inner(t, a:+false)
            }
        }
        case Nil => a
    }
    val initListBool: List[Boolean] = List()
    val res = inner(games, initListBool)
    
    return (gameNo, res)
}

def sumIds(input:List[String]): Int = {
    def inner(ls: List[String], a:Int): Int = ls match {
        case h::t => {
            val x = isGamePossible(h)
            val id = x._1
            if (x._2.contains(false)) {
                println(s"$id row not possible")
                inner(t, a)
            } else {
                val numPossible = x._2.map(bool => if (bool) 1 else 0).sum
                println(s"$id: $numPossible")
                inner(t, a+id)
            }
        }
        case Nil => a
    }
    inner(input, 0)
}
