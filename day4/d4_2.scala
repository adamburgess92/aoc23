import scala.io.Source
import scala.util.matching.Regex

// Answer is 5132675

object Main {
    def main(args: Array[String]): Unit = {
        // val test_case = readLines("day4/test.txt")
        // val ops = f(test_case)
        // val res = fl(ops)
        // res.foreach(println)

        val in = readLines("day4/data.txt")
        val ops = f(in)
        val res = fl(ops).sum
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

def numMatch(winners:List[Int], draw:List[Int]): Int = {
    var a:Int = 0
    for (d <- draw) {
        for (w <- winners) {
            if (d==w) {
                a += 1
            }
        }
    }
    a
}

def matchesInLine(input:String): Int = {
    val winners = getWinners(input)
    val draw = getDraw(input)
    val n_matches = numMatch(winners, draw)
    n_matches
}


def f(input:List[String]): List[Int] = {
    def g(input:List[String], a:List[Int]):List[Int] = input match {
        case h::t => {
            val n = matchesInLine(h) 
            g(t, a:+n)
        }
        case Nil => a
    }
    val a:List[Int] = List()
    g(input, a)
}

def update_val(a: Array[Int], value:Int, start:Int, end:Int): Array[Int] = {
    for (i <- start to end) {
        a(i) = a(i)+value
    }
    // a.foreach(println)
    // println("****")
    a
}

def fl(ops:List[Int]): List[Int] ={
    var sa = Array.fill(ops.length)(1)
    for((v, i) <- sa.zipWithIndex) {
        var max_idx = i+ops(i)
        var value = sa(i)
        sa = update_val(sa, value, i+1, max_idx)
    }
    sa.toList
}




