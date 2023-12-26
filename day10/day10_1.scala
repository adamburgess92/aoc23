import scala.io.Source

object DayTen {
    def main(args: Array[String]): Unit = {
        val grid = makeGrid(readLines("day10/data.txt"))
        val res = process(grid)
        println(res)
    }
}

def readLines(path:String): List[String] = {
    val source = Source.fromFile(path)
    val lines = try source.getLines().toList
    finally source.close()
    lines
}

def makeGrid(input: List[String]): List[List[String]] = {
    def inner(ls: List[String], a: List[List[String]]): List[List[String]] = ls match {
        case h::t => {
            val s = h.split("").toList
            inner(t, a:+s)
        }
        case Nil => a
    }
    val out = inner(input, List(List()))
    out.slice(1, out.length)
}

case class Point(rowNum: Int, colNum: Int)

def findS(input: List[List[String]]): Point = {
    def findRow(input: List[List[String]], r:Int): Int = input match {
        case h::t => {
            if (h.contains("S")) {
                r
            } else {
                findRow(t, r+1)
            }
        }
        case Nil => {
            9999
        }
    }
    def findCol(input: List[String], c: Int): Int = input match {
        case h::t => {
            if (h.contains("S")) {
                c
            } else {
                findCol(t, c+1)
            }
        }
        case Nil => 9999
    }
    Point(findRow(input, 0), findCol(input(findRow(input, 0)), 0))
}

case class Crawler(pos: Point, visited: List[Point], steps: Int, grid: List[List[String]]) {
    // Points it already visited:
    val newVisited: List[Point] = visited :+ pos

    val rowUp = pos.rowNum-1
    val rowDown = pos.rowNum+1
    val colLeft = pos.colNum-1
    val colRight = pos.colNum+1
    val rowMin = 0
    val rowMax = grid.length-1
    val colMin = 0
    val colMax = grid(0).length-1

    val upPossible = {
        if (rowUp >= rowMin && rowUp <= rowMax) {
            val symUp = grid(rowUp)(pos.colNum)
            if (List("F", "7", "|").contains(symUp) && !visited.contains(Point(pos.rowNum-1, pos.colNum))) true else false
        } else {
            false
        }
    }
    val downPossible = {
        if (rowDown >=rowMin && rowDown <= rowMax) {
            val symDown = grid(rowDown)(pos.colNum)
            if (List("J", "L", "|").contains(symDown) && !visited.contains(Point(pos.rowNum+1, pos.colNum))) true else false
        } else {
            false
        }
    }
    val leftPossible = {
        if (colLeft >= colMin && colLeft <= colMax) {
            val symLeft = grid(pos.rowNum)(colLeft)
            if (List("F", "L", "-").contains(symLeft) && !visited.contains(Point(pos.rowNum, pos.colNum-1))) true else false
        } else {
            false
        }
    }
    val rightPossible = {
        if (colRight >= colMin && colRight <= colMax) {
            val symRight = grid(pos.rowNum)(colRight)
            if (List("J", "7", "-").contains(symRight) && !visited.contains(Point(pos.rowNum, pos.colNum+1))) true else false
        } else {
            false
        }
    }

    def getOptions: List[Point] = {
        var out = List[Point]()
        if (upPossible) {
            out = out :+ Point(pos.rowNum-1, pos.colNum)
        }
        if (downPossible) {
            out = out :+ Point(pos.rowNum+1, pos.colNum)
        }
        if (leftPossible) {
            out = out :+ Point(pos.rowNum, pos.colNum-1)
        }
        if (rightPossible) {
            out = out :+ Point(pos.rowNum, pos.colNum+1)
        }
        out
    }

    def takeStep: List[Crawler] = {
        def inner(ops:List[Point], a:List[Crawler]): List[Crawler] = ops match {
            case h::t => {
                inner(t, a:+Crawler(h, newVisited, steps+1, grid))
            }
            case Nil => a
        }
        val ops = getOptions
        inner(ops, List[Crawler]())
    }
}

def samePoint(in: List[Point]): Boolean = in match {
    case Nil => true
    case h::t => t.forall(_ == h)
}

def process(grid: List[List[String]]): Int = {
    val startLocation = findS(grid)
    println(s"Starting at $startLocation")
    // grid.foreach(println)
    val startCrawler = Crawler(startLocation, List[Point](), 0, grid)
    val crawlers: List[Crawler] = List(startCrawler)

    // For each crawler: 
        // if pos are all the same, and count>0, return count

    def inner(crawlers:List[Crawler], a: Int): Int = {
        val points: List[Point] = crawlers.map(_.pos)
        if (a>0 && samePoint(points)) {
            val finalPosition = crawlers(0).pos
            println(s"Final point: $finalPosition")
            a
        } else {

            val newCrawlers: List[Crawler] = crawlers.flatMap(_.takeStep)
            inner(newCrawlers, a+1)
        }
    }
    inner(crawlers, 0)
}



