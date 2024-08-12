package day10 

import utils.Utils

object DayTenPartOne {
    def main(args: Array[String]): Unit = {
        val data = Utils.readLines("day10/data.txt")
        val grid = makeGrid(data)
        val s = findS(grid)
        val res = getFurthestDist(grid, s)
        println(res)
    }
}

case class Point(rowNum: Int, colNum: Int)

def makeGrid(input: List[String]): List[List[Char]] = {
    def inner(ls: List[String], a: List[List[Char]]): List[List[Char]] = ls match {
        case h::t => {
            val s = h.toList
            inner(t, a:+s)
        }
        case Nil => a
    }
    val out = inner(input, List(List()))
    out.slice(1, out.length)
}

def findS(input: List[List[Char]]): Point = {
    def findRow(input: List[List[Char]], r:Int): Int = input match {
        case h::t => {
            if (h.contains('S')) {
                r
            } else {
                findRow(t, r+1)
            }
        }
        case Nil => {
            9999
        }
    }
    def findCol(input: List[Char], c: Int): Int = input match {
        case h::t => {
            if (h == 'S') {
                c
            } else {
                findCol(t, c+1)
            }
        }
        case Nil => 9999
    }
    Point(findRow(input, 0), findCol(input(findRow(input, 0)), 0))
}

def checkSurroundings(grid: List[List[Char]], rowNum: Int, colNum: Int): List[Point] = {
    // Return points that can actuall be travelled to: 
    def queryGrid(rowNum: Int, colNum: Int): Option[Point] = {
        if (rowNum>=0 && colNum>=0 && rowNum<grid.length && colNum<grid(0).length) {
            Some(Point(rowNum, colNum))
        } else {
            None
        }
    }
    val current = grid(rowNum)(colNum)
    val upValid: Option[Point] = {
        if (rowNum-1<0) { 
            None
        } else if (List('|', 'J', 'L', 'S').contains(current) && List('|', 'F', '7', 'S').contains(grid(rowNum-1)(colNum))) {
            queryGrid(rowNum-1, colNum)
        } else None
    }
    val downValid: Option[Point] = {
        if (rowNum+1>grid.length) {
            None
        } else if (List('|', 'F', '7', 'S').contains(current) && List('|', 'L', 'J', 'S').contains(grid(rowNum+1)(colNum))) {
            queryGrid(rowNum+1, colNum)
        } else None
    }
    val leftValid: Option[Point] = {
        if (colNum-1<0) {
            None
        } else if (List('-', 'J', '7', 'S').contains(current) && List('-', 'L', 'F', 'S').contains(grid(rowNum)(colNum-1))) {
            queryGrid(rowNum, colNum-1)
        } else None
    }
    val rightValid: Option[Point] = {
        if (colNum+1>grid(0).length) {
            None
        } else if (List('-', 'L', 'F', 'S').contains(current) && List('-', 'J', '7', 'S').contains(grid(rowNum)(colNum+1))) {
            queryGrid(rowNum, colNum+1)
        } else None
    }
    val v: List[Option[Point]] = List(upValid, downValid, leftValid, rightValid)
    v.flatten

}

def traverseLoop(grid: List[List[Char]], startPoint: Point): List[Point] = {    
    def inner(currentPoint: Point, visitedPoints: List[Point]): List[Point] = {
        val adjacent = checkSurroundings(grid, currentPoint.rowNum, currentPoint.colNum)
        val nextPoint = adjacent.filter(!visitedPoints.contains(_)) // Filter out points already visited
        if (nextPoint.length==0) { // If nowhere left to visit, loop exhausted
            visitedPoints:+currentPoint
        } else {
            inner(nextPoint(0), visitedPoints:+currentPoint)
        }
    }
    val visitedPointsInit: List[Point] = Nil
    inner(startPoint, visitedPointsInit)
}

def getFurthestDist(grid: List[List[Char]], startPoint: Point): Int = {
    val loopPoints = traverseLoop(grid, startPoint)
    loopPoints.length/2
}