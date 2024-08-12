package day10 

import utils.Utils

object DayTenPartTwo {
    def main(args: Array[String]): Unit = {
        val data = Utils.readLines("day10/test_data5.txt")
        val grid = makeGrid(data)
        val s = findS(grid)
        val loop = traverseLoop(grid, s)
        val res = allSlices(data, grid, loop)
        println("~~~")
        println(res)
    }
}

def getLoopPartsToRight(p: Point, loop: List[Point]): List[Point] = {
    loop.filter(i => i.rowNum==p.rowNum && i.colNum>p.colNum)
}

def getChar(grid: List[List[Char]], p: Point): Char = {
    grid(p.rowNum)(p.colNum)
}

def slice(ls_char: List[Char], grid: List[List[Char]], loop: List[Point], rowNum: Int): Int = {    
    def inner(ls_char: List[Char], colNum: Int, a: Int): Int = ls_char match {
        case h::t => {
            // println(h)
            val p = Point(rowNum, colNum)
            if (loop.contains(p)) { // If part of the loop, move on...
                inner(t, colNum+1, a)
            } else {
                val loopToRight = getLoopPartsToRight(p, loop)
                val r = loopToRight.map(getChar(grid, _))
                val nCross = r.filter(List('|', 'L', 'J').contains(_)).length
                if (nCross%2==0) { // Outside loop
                    inner(t, colNum+1, a)
                } else { // Inside loop
                    inner(t, colNum+1, a+1)
                }
            }
        }
        case Nil => a
    }
    inner(ls_char, 0, 0)
}

def allSlices(data: List[String], grid: List[List[Char]], loop: List[Point]): Int = {
    def inner(data: List[String], rowNum: Int, a:Int): Int = data match {
        case h::t => {
            val b = slice(h.toList, grid, loop, rowNum)
            println(b)
            inner(t, rowNum+1, a+b)
        }
        case Nil => a
    }
    inner(data, 0, 0)
}

