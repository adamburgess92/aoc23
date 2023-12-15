import scala.io.Source
import scala.util.matching.Regex

object DayTwo {
    def main(args: Array[String]): Unit = {
        val data = readFile("day2/data.txt")
        // val data = readFile("day2/test_data.txt")
        val res = processAll(data)
        println(res)        
    }
}

def readFile(path:String): List[String] = {
    val source = Source.fromFile(path)
    val lines = try source.getLines().toList
    finally source.close()
    lines
}

def getNumRed(input:String): Option[Int] = {
    val regRed: Regex = """(\d*)\sred""".r
    val numRed = regRed.findFirstMatchIn(input).map(_.group(1)).map(_.toInt)
    numRed
}

def getNumGreen(input:String): Option[Int] = {
    val regGreen: Regex = """(\d*)\sgreen""".r
    val numGreen = regGreen.findFirstMatchIn(input).map(_.group(1)).map(_.toInt)
    numGreen
}

def getNumBlue(input:String): Option[Int] = {
    val regBlue: Regex = """(\d*)\sblue""".r
    val numBlue = regBlue.findFirstMatchIn(input).map(_.group(1)).map(_.toInt)
    numBlue
}

def getLarger(a: Option[Int], b:Option[Int]): Option[Int] = (a, b) match {
    case (Some(a), Some(b)) => Some(a.max(b))
    case (Some(a), None) => Some(a)
    case (None, Some(b)) => Some(b)
    case (None, None) => None
}

case class GameState(numRed:Option[Int], numGreen:Option[Int], numBlue:Option[Int]) {
    def returnRed: Int = {
        numRed.getOrElse(0)
    }
    def returnGreen: Int = {
        numGreen.getOrElse(0)
    }
    def returnBlue: Int = {
        numBlue.getOrElse(0)
    }
}

def minCubes(input:String): GameState = {
    val splitStr = input.split("[;|:]")
    
    val games = splitStr.slice(1, splitStr.length).toList

    val initRed:Option[Int] = None
    val initGreen:Option[Int] = None
    val initBlue:Option[Int] = None

    def inner(ls:List[String], state:GameState): GameState = ls match {
        case h::t => {
            val numRedH = getNumRed(h)
            val numGreenH = getNumGreen(h)
            val numBlueH = getNumBlue(h)

            val numRed = getLarger(numRedH, state.numRed)
            val numGreen = getLarger(numGreenH, state.numGreen)
            val numBlue = getLarger(numBlueH, state.numBlue)

            inner(t, GameState(numRed, numGreen, numBlue))
        }
        case Nil => {
            state
        }
    }
    inner(games, GameState(initRed, initGreen, initBlue))
}

def rowPower(input:String): Int = {
    val fs: GameState = minCubes(input)
    fs.returnRed * fs.returnGreen * fs.returnBlue
}

def processAll(input: List[String]): Int = {
    def inner(ls: List[String], a:Int): Int = ls match {
        case h::t => {
            val rp = rowPower(h)
            inner(t, a+rp)
        }
        case Nil => {
            a
        }
    }
    inner(input, 0)
}

