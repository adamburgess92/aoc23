package day16

import utils.Utils

object DaySixteenPartOne {
    def main(args: Array[String]): Unit = {
        val data = Utils.readLines("day16/test_data.txt")
        data.foreach(println)
        val grid = atomiseData(data)
        val beams = getBeams(Beam(Point(0, 0), 'R'), grid)
        val res = getNumVisitedPoint(beams)
        println(s"Num visited points: $res")
    }
}

def atomiseData(data: List[String]): List[List[Char]] = {
    def inner(data: List[String], a: List[List[Char]]): List[List[Char]] = data match {
        case h::t => inner(t, a:+h.toList)
        case Nil => a
    }
    val aInit: List[List[Char]] = Nil
    inner(data, aInit)
}

case class Point(row: Int, col: Int)
case class Beam(currentPoint: Point, directionOfTravel: Char) {
    def getCurrentChar(grid: List[List[Char]]): Char = {
        grid(currentPoint.row)(currentPoint.col)
    }
    def getNextTiles(grid: List[List[Char]]): List[(Point, Char)] = {
        val currentChar = getCurrentChar(grid)
        val lu: Map[(Char, Char), List[(Point, Char)]] = Map(
            ('U', '-') -> List((Point(currentPoint.row, currentPoint.col-1), 'L'), (Point(currentPoint.row, currentPoint.col+1), 'R')), 
            ('D', '-') -> List((Point(currentPoint.row, currentPoint.col-1), 'L'), (Point(currentPoint.row, currentPoint.col+1), 'R')), 
            ('L', '-') -> List((Point(currentPoint.row, currentPoint.col-1), 'L')),
            ('R', '-') -> List((Point(currentPoint.row, currentPoint.col+1), 'R')),
            
            ('U', '|') -> List((Point(currentPoint.row-1, currentPoint.col), 'U')),
            ('D', '|') -> List((Point(currentPoint.row+1, currentPoint.col), 'D')),
            ('L', '|') -> List((Point(currentPoint.row-1, currentPoint.col), 'U'), (Point(currentPoint.row+1, currentPoint.col), 'D')),
            ('R', '|') -> List((Point(currentPoint.row-1, currentPoint.col), 'U'), (Point(currentPoint.row+1, currentPoint.col), 'D')),
            
            ('U', '\\') -> List((Point(currentPoint.row, currentPoint.col-1), 'L')),
            ('D', '\\') -> List((Point(currentPoint.row, currentPoint.col+1), 'R')),
            ('L', '\\') -> List((Point(currentPoint.row-1, currentPoint.col), 'U')),
            ('R', '\\') -> List((Point(currentPoint.row+1, currentPoint.col), 'D')),
            
            ('U', '/') -> List((Point(currentPoint.row, currentPoint.col+1), 'R')),
            ('D', '/') -> List((Point(currentPoint.row, currentPoint.col-1), 'L')),
            ('L', '/') -> List((Point(currentPoint.row+1, currentPoint.col), 'D')),
            ('R', '/') -> List((Point(currentPoint.row-1, currentPoint.col), 'U')),

            ('U', '.') -> List((Point(currentPoint.row-1, currentPoint.col), 'U')),
            ('D', '.') -> List((Point(currentPoint.row+1, currentPoint.col), 'D')),
            ('L', '.') -> List((Point(currentPoint.row, currentPoint.col-1), 'L')),
            ('R', '.') -> List((Point(currentPoint.row, currentPoint.col+1), 'R')),
        )
        val lsNextPoint = lu((directionOfTravel, currentChar))
        // Make sure points are on grid
        lsNextPoint.filter(i => i._1.row>=0 && i._1.row<grid.length && i._1.col>=0 && i._1.col<grid(0).length)
    }
}

def step(current: List[Beam], visited: List[Beam], grid: List[List[Char]]): (List[Beam], List[Beam]) = {    
    val next = current.map { i => 
        i.getNextTiles(grid) 
    }.flatten.map { i => 
        Beam(Point(i._1.row, i._1.col), i._2)
    }.filter { i => // Filter things in "next" which aren't in `visited`
        !visited.contains(i)
    }    
    (next, visited++current)
}

def getBeams(startBeam: Beam, grid: List[List[Char]]): List[Beam] = {
    def inner(current: List[Beam], visited: List[Beam]): List[Beam] = {
        if (current.length==0) {
            visited
        } else {
            val (next, newVisited) = step(current, visited, grid)
            inner(next, newVisited)
        }
    }
    inner(List(startBeam), Nil)
}

def getNumVisitedPoint(lsBeams: List[Beam]): Int = {
    lsBeams.map { i =>
        Point(i._1.row, i._1.col)
    }.distinct.length
}