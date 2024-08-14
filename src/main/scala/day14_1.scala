package day14

import utils.Utils

object DayFourteenPartOne {
    def main(args: Array[String]): Unit = {
        val data = Utils.readLines("day14/data.txt")
        val grid = atomiseData(data)
        val tiltedGrid = exhaustMoveStones(grid, queryNorth)
        val res = calcLoad(tiltedGrid)
        println(res)
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

def queryNorth(grid: List[List[Char]], i: Int, j: Int): Option[Char] = {
    try {
        Some(grid(i-1)(j))
    } catch {
        case e: IndexOutOfBoundsException => None
    }
}


def movePossible(grid: List[List[Char]], i: Int, j:Int, f:(List[List[Char]], Int, Int)=>Option[Char]): Boolean = {
    grid(i)(j)=='O' && f(grid, i, j).contains('.')
}

def moveStone(grid: List[List[Char]], i: Int, j: Int, f:(List[List[Char]], Int, Int)=>Option[Char]): (List[List[Char]], Int) = {
    val (newGrid, nMoved) = if (movePossible(grid, i, j, f)) {
        val updatedRowAbove = grid(i-1).updated(j, 'O')
        val updatedRow = grid(i).updated(j, '.')
        (grid.updated(i-1, updatedRowAbove).updated(i, updatedRow), 1)
    } else {
        (grid, 0)
    }
    (newGrid, nMoved)
}

def moveAllStonesByOne(grid: List[List[Char]], f:(List[List[Char]], Int, Int)=>Option[Char]): (List[List[Char]], Int)= {
    val i = (0 to grid.length-1).toList
    val j = (0 to grid(0).length-1).toList
    val z = i.flatMap(x => j.map(y => (x, y)))
    def inner(z: List[(Int, Int)], g: List[List[Char]], nMoves:Int): (List[List[Char]], Int) = z match {
        case h::t => {
            val (newGrid, nMoved) = moveStone(g, h._1, h._2, f)
            inner(t, newGrid, nMoves+nMoved)
        }
        case Nil => (g, nMoves)
    }
    val (newGrid, nMoves) = inner(z, grid, 0)
    (newGrid, nMoves)
}

def exhaustMoveStones(grid: List[List[Char]], f: (List[List[Char]], Int, Int)=>Option[Char]): List[List[Char]] = {
    val (newGrid, nMoves) = moveAllStonesByOne(grid, f)
    // println(s"Moved $nMoves stones")
    if (nMoves==0) {
        newGrid
    } else {
        exhaustMoveStones(newGrid, f)
    }
}

def calcLoad(tiltedGrid: List[List[Char]]): Int = {
    def inner(tiltedGrid: List[List[Char]], w:Int, a:Int): Int = tiltedGrid match {
        case h::t => {
            // h = List(O,O,O,O,.,#,.,O,.,.)
            // Count stones
            val nStones = h.count(i=> i=='O')
            inner(t, w-1, a+nStones*w)
        }
        case Nil => a
    }
    inner(tiltedGrid, tiltedGrid.length, 0)
}