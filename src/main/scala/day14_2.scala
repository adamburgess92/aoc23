package day14

import utils.Utils

object DayFourteenPartTwo {
    def main(args: Array[String]): Unit = {
        val data = Utils.readLines("day14/data.txt")
        val grid = atomiseData(data)
        nCycle(10000, grid)
    }
}

def rotate(grid: List[List[Char]]): List[List[Char]] = {
    val t = grid.transpose
    t.map(_.reverse)
}

def cycle(grid: List[List[Char]]): List[List[Char]] = {
    // North: 
    val n = exhaustMoveStones(grid, queryNorth)
    // West
    val w = exhaustMoveStones(rotate(n), queryNorth)
    // South
    val s = exhaustMoveStones(rotate(w), queryNorth)
    // East
    val e = exhaustMoveStones(rotate(s), queryNorth)
    rotate(e)
}

def nCycle(n: Int, g: List[List[Char]]): Unit = {
    def inner(a: Int, g:List[List[Char]]): Unit = {
        if (a==n) {
            println("done")
        } else {
            val newGrid = cycle(g)
            val load = calcLoad(newGrid)
            println(s"$a: $load")
            inner(a+1, newGrid)
        }
    }
    inner(0, g)
}

// def nCycle(n: Int, grid: List[List[Char]]): List[List[Char]] = {
//     val cycled = (1 to n).foldLeft(grid)((a,_) => cycle(a))
//     cycled
// }
