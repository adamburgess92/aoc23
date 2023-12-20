import scala.io.Source
import scala.util.matching.Regex

object DayNine {
    def main(args: Array[String]): Unit = {
        val data = readLines("day9/data.txt")
        val res = processAllRows(data)
        println(res)
        // val res = doTheThing(data(4))
        // println(res)
    }
}

def readLines(path:String): List[String] = {
    val source = Source.fromFile(path)
    val lines = try source.getLines().toList
    finally source.close()
    lines
}

def rowToListInt(input: String): List[Int] = {
    val strArray = input.split(" ").toList
    strArray.map(_.toInt)
}

def getDiff(input: List[Int]): List[Int] = {
    def inner(ls: List[Int], a: List[Int], prev: Option[Int]): List[Int] = ls match {
        case h::t => prev match {
            case None => inner(t, a, Some(h))
            case Some(p) => {
                val diff = h - p
                inner(t, a:+diff, Some(h))
            }
        }
        case Nil => a
    }
    inner(input, List(), None)
}

def getLast(input:List[Int]): Int = {
    input.last
}

def getSum(input:List[Int]): Int = {
    input.foldLeft(0)(_+_)
}

def getDepth(input:List[Int]): Int = {
    input.foldLeft(0)(_+_)
}

case class State(a: List[List[Int]]) {
    def addLayer: State = {
        val newLayer = getDiff(a.last)
        State(a:+newLayer)
    }
    def rowSum: Int = {
        getSum(a.last)
    }
    def allZero: Boolean = {
        a.last.forall(_ == 0)
    }
}

def doTheThing(input: String): Int = {
    val ls =  rowToListInt(input)
    val initState = State(List(ls))
    // val sumRow = initState.rowSum
    // println(sumRow)

    def inner(s:State, firsts:List[Int]): List[Int] = {
        s.allZero match {
            case true => {
                firsts
            }
            case false => {
                inner(s.addLayer, firsts:+s.a.last.head)
            }
        }
    }
    val r = inner(initState, List())
    // r.foreach(println)
    // r.foldLeft(0)(_-_)
    val firstVal = r(0) 
    val theRest = r.slice(1, r.length)
    var out = firstVal

    for ((elem, i)<-theRest.zipWithIndex) {
        if (i==0 || i%2==0) {
            out -= elem
        } else {
            out += elem
        }
    }
    println(out)
    out
}

def processAllRows(input:List[String]): Int = {
    def inner(ls: List[String], a:List[Int]): List[Int] = ls match {
        case h::t => {
            inner(t, a:+doTheThing(h))
        }
        case Nil => a
    }
    inner(input, List()).foldLeft(0)(_+_)
}

