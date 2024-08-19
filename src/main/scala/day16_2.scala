package day16

import utils.Utils

object DaySixteenPartTwo {
    def main(args: Array[String]): Unit = {
        val data = Utils.readLines("day16/test_data.txt")
        data.foreach(println)
        val grid = atomiseData(data)
        val startBeams = getStartBeams(grid)
        val res = getMax(startBeams, grid)
        println(res)
    }
}

def solver(startBeam: Beam, grid: List[List[Char]]): Int = {
    val beams = getBeams(startBeam, grid)
    getNumVisitedPoint(beams)
}

def getStartBeams(grid: List[List[Char]]): List[Beam] = {
    val topRow = grid(0)
    val bottomRow = grid(grid.length-1)
    val topBeams = (0 to topRow.length-1).map { i =>
        Beam(Point(0, i), 'D')
    }.toList
    val bottomBeams = (0 to bottomRow.length-1).map { i=> 
        Beam(Point(grid.length-1, i), 'U')
    }.toList
    val leftEdge = grid.indices.map { row =>
        Beam(Point(row, 0), 'R')
    }.toList
    val rightEdge = grid.indices.map { row => 
        Beam(Point(row, grid.length-1), 'L')    
    }.toList
    topBeams ++ bottomBeams ++ leftEdge ++rightEdge
}

def getMax(lsStartBeams: List[Beam], grid: List[List[Char]]): Int = {
    def inner(lsStartBeams: List[Beam], m: Int): Int = lsStartBeams match {
        case h::t => {
            val n = solver(h, grid)
            println(s"Start point: $h")
            println(s"n: $n")
            if (n>m) {
                inner(t, n)
            } else {
                inner(t, m)
            }
        }
        case Nil => m
    }
    inner(lsStartBeams, 0)
}